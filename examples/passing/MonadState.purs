module Main where

import Prelude
import Control.Monad.Eff.Console

data Tuple a b = Tuple a b

instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show (Tuple a b) = "(" <> show a <> ", " <> show b <> ")"

class Monad m <= MonadState s m where
  get :: m s
  put :: s -> m Unit

data State s a = State (s -> Tuple s a)

runState s (State f) = f s

instance functorState :: Functor (State s) where
  map = liftM1

instance applyState :: Apply (State s) where
  apply = ap

instance applicativeState :: Applicative (State s) where
  pure a = State $ \s -> Tuple s a

instance bindState :: Bind (State s) where
  bind f g = State $ \s -> case runState s f of
                              Tuple s1 a -> runState s1 (g a)

instance monadState :: Monad (State s)

instance monadStateState :: MonadState s (State s) where
  get = State (\s -> Tuple s s)
  put s = State (\_ -> Tuple s unit)

-- Without the call to same, the following strange (but correct, in the absence of
-- functional dependencies) type:
--
-- forall m t1 t2.
-- ( Bind m
-- , MonadState t1 m
-- , MonadState t2 m
-- ) => (t1 -> t2) -> m Unit
--
-- With the type hint, the inferred type is more sensible:
--
-- forall m t.
-- ( Bind m
-- , MonadState t m
-- ) => (t -> t) -> m Unit
modify f =
  do
    s <- get
    put (same f s)
  where
    same :: forall a. (a -> a) -> (a -> a)
    same = id

main = do
  logShow $ runState 0 (modify (_ + 1))
  log "Done"
