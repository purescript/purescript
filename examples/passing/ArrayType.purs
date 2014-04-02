module Main where

class Pointed p where
  point :: forall a. a -> p a

instance pointedArray :: Pointed [] where
  point a = [a]

main = Debug.Trace.trace "Done"
