module Mod14 where

data D a = D0 | D1 a

class C a where
  c0 :: a
  c1 :: a

instance cInt :: C Int where
  c0 = 0
  c1 = 0

instance cD :: C a => C (D a) where
  c0 = D0
  c1 = D1 c0

d0 :: Int
d0 = c0

d1 :: Int
d1 = c1
