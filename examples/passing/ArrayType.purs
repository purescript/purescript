module ArrayType where

test :: [] Number
test = [1, 2, 3]

class Functor f where
  fmap :: forall a b. (a -> b) -> f a -> f b

foreign import (:) :: forall a. a -> [a] -> [a]

instance Functor [] where
  fmap _ [] = []
  fmap f (x:xs) = f x : fmap f xs
    
module Main where

main = Trace.trace "Done"
