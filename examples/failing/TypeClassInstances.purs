module Main where

class A a where
  a :: a -> String
  b :: a -> Number

instance aString :: A String where
  a s = s
