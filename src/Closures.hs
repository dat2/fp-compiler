-- | This module is about converting a naive CoreExpr into a CoreProgram with closures
-- lifted out.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module Closures
    ( liftClosures
    ) where

import Control.Monad.State.Lazy
       (MonadState, evalStateT, get, modify, put)
import Control.Monad.Writer.Lazy (MonadWriter, runWriter, tell)
import Core
import Data.Functor.Foldable (Base, cata, embed)
import Data.List (delete)
import Data.Maybe (maybe)

-- without is (\\) but removes all occurrences not just the first
without :: Eq a => [a] -> [a] -> [a]
without as bs = filter f as
  where
    f a = a `notElem` bs

-- | This returns a list of variables that are not currently bound by the lambda expr, or reference a variable that is outside of the current expr
freeVars :: CoreExpr -> [Id]
freeVars = cata alg
  where
    alg :: Base CoreExpr [Id] -> [Id]
    alg (VarF s) = [s]
    alg (LitF _) = []
    alg (AppF l r) = l ++ r
    alg (LamF v acc) = delete v acc

-- | Apply a lambda expr to a list of core vars
-- eg. apply (\x \y + x y) [y] = ((\x \y + x y) y)
apply :: CoreExpr -> [Id] -> CoreExpr
apply e (v:vs) = apply (App e $ Var v) vs
apply e [] = e

-- | The opposite of apply
wrapLambdas :: [Id] -> CoreExpr -> CoreExpr
wrapLambdas (v:vs) e = wrapLambdas vs (Lam v $ e)
wrapLambdas [] e = e

-- | Convert lambdas that have free variables into lambdas that "close" their environment, so no nested expression will have any free variables
-- (eg. \x \y + x y => (\x (\x y + x y) x))
convertIntoClosures :: [Id] -> CoreExpr -> CoreExpr
convertIntoClosures globals = cata alg
  where
    alg :: Base CoreExpr CoreExpr -> CoreExpr
    alg (LamF v b) =
        let env = freeVars b `without` (globals ++ [v])
        in wrapLambdas env (Lam v b) `apply` env
    alg x = embed x

data NameState = NameState
    { suggestion :: Maybe String
    , count :: Int
    }

initNameState :: NameState
initNameState = NameState {suggestion = Nothing, count = 0}

suggest :: MonadState NameState m => String -> m ()
suggest s = put $ NameState {suggestion = Just s, count = 0}

-- This will generate a fresh name for a definition
fresh :: MonadState NameState m => m Id
fresh = do
    NameState {suggestion, count} <- get
    put (NameState {suggestion = suggestion, count = count + 1})
    return $ Id $ "_" ++ maybe "" (++ "_") suggestion ++ show count

-- | This will lift expressions into a list of bindings
liftLambdasIntoBindings ::
       forall m. (MonadWriter [CoreBind] m, MonadState NameState m)
    => CoreExpr
    -> m CoreExpr
liftLambdasIntoBindings = cata alg
  where
    alg :: Base CoreExpr (m CoreExpr) -> m CoreExpr
    alg (VarF s) = return $ Var s
    alg (LitF i) = return $ Lit i
    alg (AppF ml mr) = do
        l <- ml
        r <- mr
        return $ App l r
    alg (LamF v me) = do
        name <- fresh
        e <- me
        tell [CoreBind name (Lam v e)]
        return $ Var name

-- This will take all lambdas, and generate a list of definitions for each lambda
-- lift inner defined lambdas and closures into upper level definitions
liftClosures :: CoreProgram -> CoreProgram
liftClosures bindings =
    let globals = map Id ["+", "-", "/", "*"]
        mapBinding (CoreBind (Id n) e) =
            CoreBind (Id n) <$>
            (suggest n >>
             liftLambdasIntoBindings (convertIntoClosures globals e))
        bindingsM' = mapM mapBinding bindings
        (bindings', newBindings) =
            runWriter $ evalStateT bindingsM' initNameState
    in bindings' ++ newBindings
