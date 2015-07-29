module Main where

import Prelude

class A a where
  a :: a -> Boolean

class B a where
  b :: a -> Boolean

instance aNumber :: A Number where
  a 0.0 = true
  a n = b (n - 1.0)

instance bNumber :: B Number where
  b 0.0 = false
  b n = a (n - 1.0)

main = Control.Monad.Eff.Console.print $ a 10.0
