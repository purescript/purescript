module Main where

import Prelude ()
import Data.Array

test :: [] Number
test = [1, 2, 3]

class Functor f where
  fmap :: forall a b. (a -> b) -> f a -> f b

instance functorArray :: Functor [] where
  fmap _ [] = []
  fmap f (x:xs) = f x : fmap f xs

main = Debug.Trace.trace "Done"
