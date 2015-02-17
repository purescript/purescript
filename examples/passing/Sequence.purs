module Main where

import Control.Monad.Eff

class Sequence t where
  sequence :: forall m a. (Monad m) => t (m a) -> m (t a)

instance sequenceArray :: Sequence [] where
  sequence [] = pure []
  sequence (x:xs) = (:) <$> x <*> sequence xs

main = sequence $ [Debug.Trace.trace "Done"]
