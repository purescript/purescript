module Main where

import Prelude

data Either a b = Left a | Right b

instance functorEither :: Prelude.Functor (Either a) where
  (<$>) _ (Left x) = Left x
  (<$>) f (Right y) = Right (f y)

main = Debug.Trace.trace "Done"
