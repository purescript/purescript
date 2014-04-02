module Main where

import Prelude

class A a where
  a :: a -> Boolean

class B a where
  b :: a -> Boolean

instance aNumber :: A Number where
  a 0 = true
  a n = b (n - 1)

instance bNumber :: B Number where
  b 0 = false
  b n = a (n - 1)

main = Debug.Trace.print $ a 10
