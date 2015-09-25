module Main where

import Prelude

data Maybe a = Nothing | Just a

instance functorMaybe :: Functor Maybe where
  map f Nothing = Nothing
  map f (Just x) = Just (f x)

instance applyMaybe :: Apply Maybe where
  apply (Just f) (Just x) = Just (f x)
  apply _ _ = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

instance bindMaybe :: Bind Maybe where
  bind Nothing _ = Nothing
  bind (Just a) f = f a

instance monadMaybe :: Prelude.Monad Maybe

test1 = \_ -> do
  Just "abc"

test2 = \_ -> do
  x <- Just 1.0
  y <- Just 2.0
  Just (x + y)

test3 = \_ -> do
  Just 1.0
  Nothing :: Maybe Number
  Just 2.0

test4 mx my = do
  x <- mx
  y <- my
  Just (x + y + 1.0)

test5 mx my mz = do
  x <- mx
  y <- my
  let sum = x + y
  z <- mz
  Just (z + sum + 1.0)

test6 mx = \_ -> do
  let
    f :: forall a. Maybe a -> a
    f (Just x) = x
  Just (f mx)

test8 = \_ -> do
  Just (do
    Just 1.0)

test9 = \_ -> (+) <$> Just 1.0 <*> Just 2.0

test10 _ = do
  let
    f x = g x * 3.0
    g x = f x / 2.0
  Just (f 10.0)

main = Control.Monad.Eff.Console.log "Done"
