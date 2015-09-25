module Main where

import Prelude

class Test a where
  fn :: a -> a -> a
  val :: a

instance testBoolean :: Test Boolean where
  val = true
  fn x y = y

main = Control.Monad.Eff.Console.log (show (fn true val))
