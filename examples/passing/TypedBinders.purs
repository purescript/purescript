module Main where

import Prelude

data Tuple a b = Tuple a b

class MonadState s m where
  get :: m s
  put :: s -> m {}

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
  put s = State (\_ -> Tuple s {})

modify :: forall m s. (Prelude.Monad m, MonadState s m) => (s -> s) -> m {}
modify f = do
  s <- get
  put (f s)

test :: Tuple String String
test = runState "" $ do
  modify $ (++) "World!"
  modify $ (++) "Hello, "
  str :: String <- get
  return str

test2 :: (Int -> Int) -> Int
test2 = (\(f :: Int -> Int) -> f 10)

test3 :: Int -> Boolean 
test3 n = case n of
  (0 :: Int) -> true
  _ -> false

test4 :: Tuple Int Int -> Tuple Int Int 
test4 = (\(Tuple a b :: Tuple Int Int) -> Tuple b a)

main = do
  let t1 = test
      t2 = test2 id
      t3 = test3 1
      t4 = test4 (Tuple 1 0)
  Control.Monad.Eff.Console.log "Done"