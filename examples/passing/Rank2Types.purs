module Rank2Types where

  import Prelude

  test1 :: (forall a. (a -> a)) -> Number
  test1 = \f -> f 0

  foreign import data ST :: * -> * -> *

  foreign import runST :: forall a. (forall s. ST s a) -> a

  foreign import exampleST :: forall s. ST s Number

  testST = \_ -> runST exampleST

  foreign import push :: forall el. el -> [el] -> [el]

  replicateM :: forall m a. (forall a. a -> m a) -> (forall a b. m a -> (a -> m b) -> m b) -> Number -> m a -> m [a]
  replicateM = \ret bind n m -> case n of
    0 -> ret []
    n -> bind m (\x -> bind (replicateM ret bind (n - 1) m) (\xs -> ret (push x xs)))
    
module Main where

main = Trace.trace "Done"
