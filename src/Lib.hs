module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
# LANGUAGE OverloadedStrings #
# LANGUAGE FlexibleContexts #
# LANGUAGE DeriveFunctor #
# LANGUAGE TypeFamilies #
# LANGUAGE ScopedTypeVariables #

module Lib
    ( Expr(..)
    , PrimOp(..)
    , compile
    ) where

import Control.Monad.State.Lazy
       (MonadState, evalStateT, get, modify, put)
import Control.Monad.Writer.Lazy (MonadWriter, runWriter, tell)
import Data.ByteString.Short (ShortByteString(..))
import Data.Functor.Foldable
       (Base(..), Corecursive(..), Recursive(..))
import Data.List ((\\), findIndex, intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid (First(..))
import Data.String (IsString(..))
import qualified Data.Text.Lazy.IO as T
import Debug.Trace
import LLVM.AST
       (BasicBlock(..), Definition(..), Module(..), Name(..), Type(..),
        functionDefaults)
import LLVM.AST.Constant (Constant(..))
import LLVM.AST.Global (Global(..))
import LLVM.AST.Instruction (Terminator(..))
import LLVM.AST.Operand (Operand(..))
import qualified LLVM.AST.Type as Ty
import LLVM.Analysis (verify)
import LLVM.Context (withContext)
import LLVM.IRBuilder.Constant (int32)
import LLVM.IRBuilder.Instruction (add, call, mul, ret, sdiv, sub)
import LLVM.IRBuilder.Module
       (ModuleBuilder, MonadModuleBuilder, ParameterName(..), buildModule,
        extern, function)
import LLVM.IRBuilder.Monad (MonadIRBuilder, fresh)
import LLVM.Module (File(..), withModuleFromAST, writeObjectToFile)
import LLVM.Pretty (ppllvm)
import LLVM.Target (withHostTargetMachine)
import System.Process (callCommand)

-- This represents operations that we support natively
data PrimOp
    = Plus
    | Subtract
    | Multiply
    | Divide
    deriving (Eq)

instance Show PrimOp where
    show Plus = "+"
    show Subtract = "-"
    show Multiply = "*"
    show Divide = "/"

-- This represents a basic expression
data Expr
    = Var String
    | Prim PrimOp
    | Lit Integer
    | App Expr
          Expr
    | Lam [String]
          Expr
    deriving (Eq)

-- % is a local variable, @ is a global variable
-- this is shamelessly stolen from llvm
instance Show Expr where
    show = cata alg
        -- base cases
      where
        alg (VarF s) = s
        alg (PrimF p) = show p
        alg (LitF i) = show i
        -- recursive cases
        alg (AppF l r) = "(" ++ l ++ " " ++ r ++ ")"
        alg (LamF vs b) =
            let params = intercalate "," vs
            in "(\\" ++ params ++ " -> " ++ b ++ ")"

-- Parameterized version of Expr
data ExprF a
    = VarF String
    | PrimF PrimOp
    | LitF Integer
    | AppF a
           a
    | LamF [String]
           a
    deriving (Show, Eq, Functor)

-- type family Base t :: * -> *
-- Base Expr * -> gives a new type
-- Base Expr [String] is the type for freeVars
-- ExprF is a member of the Base family ?
type instance Base Expr = ExprF

-- turn an Expr into an ExprF so we can do some cata morphisms on it
-- The recursive typeclass represents a recursively defined data structure
instance Recursive Expr where
    project (Var s) = VarF s
    project (Prim op) = PrimF op
    project (Lit i) = LitF i
    project (App l r) = AppF l r
    project (Lam vs b) = LamF vs b

instance Corecursive Expr where
    embed (VarF s) = Var s
    embed (LitF i) = Lit i
    embed (PrimF op) = Prim op
    embed (AppF l r) = App l r
    embed (LamF vs b) = Lam vs b

-- This represents a top level lambda
data Defn =
    Defn String
         [String]
         Expr
    deriving (Eq)

instance Show Defn where
    show (Defn name vs e) =
        let params = intercalate "," vs
        in "let " ++ name ++ " = (\\" ++ params ++ " -> " ++ show e ++ ")"

-- difference is (\\) but removes all occurrences not just the first
difference :: Eq a => [a] -> [a] -> [a]
difference as bs = filter f as
  where
    f a = a `notElem` bs

-- This returns a list of variables that are referencing parent variables
freeVars :: Expr -> [String]
freeVars = cata alg
  where
    alg :: Base Expr [String] -> [String]
  -- base cases, non recursive constructors
    alg (VarF s) = [s]
    alg (PrimF _) = []
    alg (LitF _) = []
  -- recursive cases
  -- app holds 2 lists
    alg (AppF l r) = l ++ r
  -- lam holds some vars and a list
    alg (LamF vs acc) = acc `difference` vs

-- This takes a lambda and converts it to a lambda that has no references to parent scoped
-- variables (eg. \x \y + x y => (\x (\x y + x y) x))
close :: Expr -> Expr
close = cata alg
  where
    alg :: Base Expr Expr -> Expr
    alg (LamF vs b) =
        let env = freeVars b `difference` vs
        in Lam (env ++ vs) b `apply` env
    alg x = embed x

-- Take a (Lam vs body) and a list of vs
-- and convert it into a call site
apply :: Expr -> [String] -> Expr
apply e (v:vs) = apply (App e $ Var v) vs
apply e [] = e

-- This will generate a fresh name for a definition
genName :: MonadState Int m => m String
genName = do
    s <- get
    put (s + 1)
    return $ show s

-- This will take all lambdas, and generate a list of definitions for each lambda
liftLambdas ::
       forall m. (MonadWriter [Defn] m, MonadState Int m)
    => Expr
    -> m Expr
liftLambdas = cata alg
  where
    alg :: Base Expr (m Expr) -> m Expr
  -- base cases
    alg (VarF s) = return $ Var s
    alg (PrimF op) = return $ Prim op
    alg (LitF i) = return $ Lit i
  -- recursive cases
    alg (AppF ml mr) = do
        l <- ml
        r <- mr
        return $ App l r
    alg (LamF vs me) = do
        name <- genName
        e <- me
        tell [Defn name vs e]
        return $ Var name

-- lift inner defined lambdas and closures into upper level definitions
eliminateLambdas :: Expr -> (Expr, [Defn])
eliminateLambdas e = runWriter $ evalStateT (liftLambdas (close e)) 0

smash :: Expr -> Expr
smash = cata alg
  where
    alg (LamF vs (Lam vs' e)) = Lam (vs ++ vs') e
    alg x = embed x

type GlobalState = Map.Map String Operand

initGlobalState :: GlobalState
initGlobalState = Map.empty

lookupGlobal :: String -> GlobalState -> Maybe Operand
lookupGlobal = Map.lookup

memberGlobal :: String -> GlobalState -> Bool
memberGlobal = Map.member

-- take a main expr, and a list of definitions already extracted
-- build a module
codegenModule :: Expr -> [Defn] -> Module
codegenModule mainBody defns = buildModule "anonymous" builder
  where
    builder = evalStateT builderWithState initGlobalState
    builderWithState = do
        mapM_ buildDefn defns
        buildDefn (Defn "main" [] mainBody)
        return ()

-- build a single definition
buildDefn :: (MonadState GlobalState m, MonadModuleBuilder m) => Defn -> m ()
buildDefn (Defn name vs body) = do
    globals <- get
    o <-
        function
            (Name (fromString name))
            (map makeParam vs)
            Ty.i32
            (buildDefnBody globals body vs)
    modify (Map.insert name o)
  where
    makeParam name = (Ty.i32, ParameterName (fromString name))

-- build the instructions of a definition
buildDefnBody ::
       forall m. MonadIRBuilder m
    => GlobalState
    -> Expr
    -> [String]
    -> [Operand]
    -> m ()
buildDefnBody globals expr paramNames operands = do
    r <- para alg expr
    ret r
  where
    localParamMap = Map.fromList $ zip paramNames operands
    alg :: Base Expr (Expr, m Operand) -> m Operand
    alg (LitF i) = int32 i
    alg (VarF s) = return $ findVar s
    -- when calling a global function, it will have the form
    -- App (Var globalVarName) arg
    -- We want to call that global function with a single argument :)
    alg (AppF (Var s, ml) (_, mr))
        | s `memberGlobal` globals = do
            l <- ml
            r <- mr
            call l [(r, [])]
        -- can't call a param as a function
        | otherwise = undefined
    -- when applying a primitive to a value, we need 2 cases to handle it
    -- 1) this case says return the underlying value
    alg (AppF (Prim _, _) (_, mr)) = mr
    -- 2) this case checks that we're applying a primitive to 2 values
    -- then we can trust that case 1) will return ml -> the underlying value
    -- and mr is already handled
    alg (AppF (App (Prim op) _, ml) (_, mr)) = do
        l <- ml
        r <- mr
        (getLlvmPrimOp op) l r
    -- Other operations can't really be handled
    alg _ = undefined
    findVar s =
        fromJust . getFirst $
        First (Map.lookup s localParamMap) <> First (lookupGlobal s globals)

-- return a function to generate an instruction for each prim op
getLlvmPrimOp :: MonadIRBuilder m => PrimOp -> (Operand -> Operand -> m Operand)
getLlvmPrimOp Plus = add
getLlvmPrimOp Subtract = sub
getLlvmPrimOp Multiply = mul
getLlvmPrimOp Divide = sdiv

-- make an object file, that is ready to be linked
makeObjectFile :: String -> Module -> IO ()
makeObjectFile filePath astModule = do
    withContext $ \context ->
        withModuleFromAST context astModule $ \internalModule -> do
            verify internalModule
            withHostTargetMachine $ \targetMachine ->
                writeObjectToFile targetMachine (File filePath) internalModule

-- call the system linker
linkObjectFile :: String -> String -> IO ()
linkObjectFile fileName outBinaryFileName =
    callCommand $
    "ld -macosx_version_min 10.14 -arch x86_64 -lSystem " ++
    fileName ++ " -o " ++ outBinaryFileName

-- compile a lambda expr into an executable file
compile :: Expr -> String -> IO ()
compile expr filename = do
    traceShowM expr
    let (e, defns) = eliminateLambdas (smash expr)
    traceShowM defns
    traceShowM e
    let mod = codegenModule e defns
    T.putStrLn (ppllvm mod)
    makeObjectFile (filename ++ ".o") mod
    linkObjectFile (filename ++ ".o") (filename ++ "-exe")
-}
