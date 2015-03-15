module Main where

class (Monad m) <= MonadBlah b m where
  blah :: b -> m Unit

example :: forall m b. (MonadBlah b m) => b -> m Unit
example b = do
  blah b
  blah b
  blah b

main = Debug.Trace.trace "Done"
