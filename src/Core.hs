{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Core
    ( CoreExpr(..)
    , CoreBind(..)
    , CoreProgram
    , Id(..)
    , Literal(..)
    ) where

import Data.Functor.Foldable (Recursive(..))
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.List (intercalate)

-- Identifiers
newtype Id =
    Id String
    deriving (Eq)

instance Show Id where
    show (Id s) = s

-- | Literals
data Literal =
    LInt Int
    deriving (Eq)

instance Show Literal where
    show (LInt l) = show l

-- | The core lambda calculus
data CoreExpr
    = Var Id
    | Lit Literal
    | App CoreExpr
          CoreExpr
    | Lam Id
          CoreExpr
    deriving (Eq)

makeBaseFunctor ''CoreExpr

instance Show CoreExpr where
    show = cata alg
      where
        alg (VarF s) = show s
        alg (LitF i) = show i
        -- recursive cases
        alg (AppF l r) = "(" ++ l ++ " " ++ r ++ ")"
        alg (LamF v b) = "(\\" ++ (show v) ++ " -> " ++ b ++ ")"

-- | Top Level binding
data CoreBind =
    CoreBind Id
             CoreExpr
    deriving (Eq)

instance Show CoreBind where
    show (CoreBind name e) = "let " ++ (show name) ++ " = " ++ show e

-- | Top Level Program
type CoreProgram = [CoreBind]
