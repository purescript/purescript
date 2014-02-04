module Do where

import Prelude

data Maybe a = Nothing | Just a

instance Prelude.Monad Maybe where
  ret = Just
  (>>=) Nothing _ = Nothing
  (>>=) (Just a) f = f a

test1 = \_ -> do 
  Just "abc"

test2 = \_ -> do
  (x : _) <- Just [1, 2, 3]
  (y : _) <- Just [4, 5, 6]
  Just (x + y)

test3 = \_ -> do
  Just 1
  Nothing :: Maybe Number
  Just 2

test4 mx my = do
  x <- mx
  y <- my
  Just (x + y + 1)

test5 mx my mz = do
  x <- mx
  y <- my
  let sum = x + y
  z <- mz
  Just (z + sum + 1)

test6 mx = \_ -> do 
  let Just x = mx
  Just x

test8 = \_ -> do 
  Just (do 
    Just 1)

(<$>) :: forall a b. (a -> b) -> Maybe a -> Maybe b
(<$>) f m = do
  a <- m
  ret (f a)

(<*>) :: forall a b. (Maybe (a -> b)) -> Maybe a -> Maybe b
(<*>) f m = do
  g <- f
  a <- m
  ret (g a)

forever :: forall m a b. (Monad m) => m a -> m b
forever a = do
  a
  forever a

test9 = \_ -> (+) <$> Just 1 <*> Just 2
    
module Main where

main = Trace.trace "Done"
