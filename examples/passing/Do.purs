module Prelude where

class Monad m where
  ret :: forall a. a -> m a
  (>>=) :: forall a b. m a -> (a -> m b) -> m b

module Do where

import Prelude

data Maybe a = Nothing | Just a

instance Prelude.Monad Maybe where
  ret = Just
  (>>=) Nothing _ = Nothing
  (>>=) (Just a) f = f a

test1 = do Just "abc"

test3 = do
  (x : _) <- Just [1, 2, 3]
  (y : _) <- Just [4, 5, 6]
  Just (x + y)

test4 = do
  Just 1
  Nothing :: Maybe Number
  Just 2

test5 mx my = do
  x <- mx
  y <- my
  Just (x + y)

test6 mx my mz = do
  x <- mx
  y <- my
  let sum = x + y
  z <- mz
  Just (z + sum)

test7 mx = do let Just x = mx
              Just x

test8 = do Just (do Just 1)

(<$>) :: forall a b. (a -> b) -> Maybe a -> Maybe b
(<$>) f m = do
  a <- m
  ret (f a)

(<*>) :: forall a b. (Maybe (a -> b)) -> Maybe a -> Maybe b
(<*>) f m = do
  g <- f
  a <- m
  ret (g a)

foo x y z = x + y + z

forever :: forall m a b. (Monad m) => m a -> m b
forever a = do
  a
  forever a

test9 = foo <$> Just 1 <*> Just 2 <*> Just 3
