module Main where

  import Prelude
  import Data.Array

  test1 :: (forall a. (a -> a)) -> Number
  test1 = \f -> f 0

  replicateM :: forall m a. (forall a. a -> m a) -> (forall a b. m a -> (a -> m b) -> m b) -> Number -> m a -> m [a]
  replicateM = \return bind n m -> case n of
    0 -> return []
    n -> bind m (\x -> bind (replicateM return bind (n - 1) m) (\xs -> return (x : xs)))
    
  main = Debug.Trace.trace "Done"
