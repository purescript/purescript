module Mod0 where

import Prelude

class C a where
  c0 :: a
  c1 :: Unit -> a

instance cInt :: C Int where
  c0 = 0
  c1 _ = c0

c :: Int
c = c0

f :: Unit -> Int
f = c1
