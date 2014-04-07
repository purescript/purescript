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

instance applyState :: Apply (State s) where
  (<*>) = ap

instance applicativeState :: Applicative (State s) where
  pure = return

instance bindState :: Bind (State s) where
  (>>=) f g = State $ \s -> case runState s f of
                              Tuple s1 a -> runState s1 (g a)

instance monadState :: Monad (State s) where
  return a = State $ \s -> Tuple s a

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

