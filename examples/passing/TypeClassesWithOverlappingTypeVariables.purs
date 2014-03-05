module Main where

  import Prelude
  import Data.Either

  instance functorEither :: Prelude.Functor (Either a) where
    (<$>) _ (Left x) = Left x
    (<$>) f (Right y) = Right (f y)

  main = Debug.Trace.trace "Done"
