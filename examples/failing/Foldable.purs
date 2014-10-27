module Main where

class Foldable f where
  fold :: forall a b. (a -> b -> b) -> b -> f a -> b
  size :: forall a. f a -> Number

instance foldableArray :: Foldable [] where
  fold _ z [] = z
  fold f z (x:xs) = x `f` (fold f z xs)
  size = fold (const ((+) 1)) 0

x = size [1,2,3]
