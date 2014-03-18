module Main where

import Prelude ()

class Applicative f where
  pure :: forall a. a -> f a
  (<*>) :: forall a b. f (a -> b) -> f a -> f b

data Maybe a = Nothing | Just a

instance applicativeMaybe :: Applicative Maybe where
  pure = Just
  (<*>) (Just f) (Just a) = Just (f a)
  (<*>) _ _ = Nothing

main = Debug.Trace.trace "Done"
