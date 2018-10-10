module Main where

import Closures
import Core
import Lib

plus1 = (Lam (Id "x") (App (App (Var (Id "+")) (Var (Id "x"))) (Lit (LInt 1))))

plusClosure =
    (Lam (Id "x") (App (App (Var (Id "+")) (Var (Id "x"))) (Var (Id "y"))))

main :: IO ()
main = do
    mapM_
        print
        (liftClosures
             [(CoreBind (Id "plus") plusClosure), (CoreBind (Id "plus1") plus1)])
