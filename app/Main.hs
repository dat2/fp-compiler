module Main where

import Core
import Lib

testProgram =
    [ CoreBind
          (Id "plus1")
          (Lam (Id "x") (App (App (Var (Id "+")) (Var (Id "x"))) (Lit (LInt 1))))
    ]

main :: IO ()
main = print testProgram
