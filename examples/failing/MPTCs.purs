module Main where

class Foo a where
  f :: a -> a

instance Foo String String where
  f a = a
