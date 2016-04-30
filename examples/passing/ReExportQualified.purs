module A where
  x = "Do"

module B where
  y = "ne"

module C (module A, module M2) where
  import A
  import B as M2

module Main where

  import Prelude
  import C

  main = Control.Monad.Eff.Console.log (x ++ y)
