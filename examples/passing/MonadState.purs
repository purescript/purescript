module Main where

import Prelude

data Tuple a b = Tuple a b

class MonadState s m where
  get :: m s
  put :: s -> m {}

data State s a = State (s -> Tuple s a)

runState s (State f) = f s

instance functorState :: Functor (State s) where
  (<$>) = liftM1

instance appState :: Applicative (State s) where
  pure = return
  (<*>) = ap

instance monadState :: Prelude.Monad (State s) where
  return a = State $ \s -> Tuple s a
  (>>=) f g = State $ \s -> case runState s f of
                              Tuple s1 a -> runState s1 (g a)

instance monadStateState :: MonadState s (State s) where
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

main = do
  let t1 = test
  Debug.Trace.trace "Done"

