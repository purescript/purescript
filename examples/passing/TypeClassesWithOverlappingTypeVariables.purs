module Main where

  import Prelude
  import Either

  instance Prelude.Functor (Either a) where
    (<$>) _ (Left x) = Left x
    (<$>) f (Right y) = Right (f y)

  main = Trace.trace "Done"
