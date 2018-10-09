-- | This module is about converting a naive CoreExpr into a CoreProgram with closures
-- lifted out.
module Closures
    ( convertIntoClosures
    , freeVars
    ) where

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
