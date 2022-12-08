-- @shouldFailWith EscapedSkolem
module Main where

data Proxy a = Proxy

data A (a :: forall k. k -> Type) = A

test :: Int
test = 0
  where
  type B = Proxy A
