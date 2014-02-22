module Main where

import Prelude
import Data.Tuple

class MonadState s m where
  get :: m s
  put :: s -> m {}

data State s a = State (s -> Tuple s a)

runState s (State f) = f s

instance Prelude.Monad (State s) where
  return a = State $ \s -> Tuple s a
  (>>=) f g = State $ \s -> let (Tuple s1 a) = runState s f in 
                            runState s1 (g a)

instance MonadState s (State s) where
  get = State (\s -> Tuple s s)
  put s = State (\_ -> Tuple s {})

modify :: forall m s. (Prelude.Monad m, MonadState s m) => (s -> s) -> m {}
modify f = do
  s <- get
  put (f s)

test :: Tuple String String
test = runState "" $ do
  modify $ (++) "World!"
  modify $ (++) "Hello, "
  get

main = Debug.Trace.print test

