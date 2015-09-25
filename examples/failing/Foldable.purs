-- @shouldFailWith CycleInDeclaration
module Main where

import Prelude

class Foldable f where
  fold :: forall a b. (a -> b -> b) -> b -> f a -> b
  size :: forall a. f a -> Number

data L a = C a (L a) | N

instance foldableL :: Foldable L where
  fold _ z N = z
  fold f z (C x xs) = x `f` (fold f z xs)
  size = fold (const ((+) 1.0)) 0.0

x = size (C 1 (C 2 (C 3 N)))
