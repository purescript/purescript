module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Ref (newRef, writeRef, readRef)

data Maybe a = Nothing | Just a

instance functorMaybe :: Functor Maybe where
  map f Nothing = Nothing
  map f (Just x) = Just (f x)

instance applyMaybe :: Apply Maybe where
  apply (Just f) (Just x) = Just (f x)
  apply _ _ = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

test1 = \_ -> ado
  in "abc"

test2 = \_ -> ado
  x <- Just 1.0
  y <- Just 2.0
  in x + y

test3 = \_ -> ado
  _ <- Just 1.0
  _ <- Nothing :: Maybe Number
  in 2.0

test4 mx my = ado
  x <- mx
  y <- my
  in x + y + 1.0

test5 mx my mz = ado
  x <- mx
  y <- my
  let sum = x + y
  z <- mz
  in z + sum + 1.0

test6 mx = \_ -> ado
  let
    f :: forall a. Maybe a -> a
    f (Just x) = x
  in f mx

test8 = \_ -> ado
  in (ado
    in 1.0)

test9 = \_ -> (+) <$> Just 1.0 <*> Just 2.0

test10 _ = ado
  let
    f x = g x * 3.0
    g x = f x / 2.0
  in f 10.0

test11 = \_ -> ado
  x <- pure 1
  y <- pure "A"
  z <- pure []
  in show (x :: Int) <> y <> show (z :: Array Int)

main = do
  r <- newRef "X"
  log =<< ado
    _ <- writeRef r "D"
    a <- readRef r
    b <- pure "o"
    let c = "n"
    d <- pure "e"
    in a <> b <> c <> d
