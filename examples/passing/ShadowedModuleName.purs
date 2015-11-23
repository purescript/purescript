module Test where

  data Z = Z String

  runZ :: Z -> String
  runZ (Z s) = s

module Main where

  import Test
  import Control.Monad.Eff.Console

  data Test = Test

  main = log (runZ (Z "done"))
