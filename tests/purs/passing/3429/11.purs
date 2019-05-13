module Mod11 where

import Prelude

class B a where
  x :: a

instance bUnit :: B Unit where
  x = unit

class C a where
  c0 :: a
  c1 :: a

instance cUnit :: B Unit => C Unit where
  c0 = unit
  c1 = x

d0 :: Unit
d0 = c0

d1 :: Unit
d1 = c0
