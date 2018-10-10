-- | This module is about converting a naive CoreExpr into a CoreProgram with closures
-- lifted out.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Closures
    ( liftClosures
    ) where

import Control.Monad.State.Lazy
       (MonadState, evalStateT, get, modify, put)
import Control.Monad.Writer.Lazy (MonadWriter, runWriter, tell)
import Core
import Data.Functor.Foldable (Base, cata, embed)
import Data.List (delete)

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

-- | This will lift expressions into a list of bindings
liftLambdasIntoBindings ::
       forall m. (MonadWriter [CoreBind] m, MonadState Int m)
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
        name <- genName
        e <- me
        tell [CoreBind name (Lam v e)]
        return $ Var name

-- base cases
-- recursive cases
-- This will generate a fresh name for a definition
genName :: MonadState Int m => m Id
genName = do
    s <- get
    put (s + 1)
    return $ Id $ "_" ++ show s

-- This will take all lambdas, and generate a list of definitions for each lambda
-- lift inner defined lambdas and closures into upper level definitions
liftClosures :: CoreProgram -> CoreProgram
liftClosures bindings =
    let globals = map Id ["+", "-", "/", "*"]
        mapBinding (CoreBind name e) =
            CoreBind name <$>
            liftLambdasIntoBindings (convertIntoClosures globals e)
        bindingsM' = mapM mapBinding bindings
        (bindings', newBindings) = runWriter $ evalStateT bindingsM' 0
    in bindings' ++ newBindings
