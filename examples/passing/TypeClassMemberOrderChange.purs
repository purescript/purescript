module Main where

class Test a where
  fn :: a -> a -> a
  val :: a

instance testBoolean :: Test Boolean where
  val = true
  fn x y = y

main = Debug.Trace.trace (show (fn true val))
