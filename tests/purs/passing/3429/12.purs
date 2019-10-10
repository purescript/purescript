module Mod12 where

class B

instance b :: B

class C a where
  c0 :: a
  c1 :: B => a

instance cInt :: C Int where
  c0 = 0
  c1 = c0

d0 :: Int
d0 = c0

d1 :: Int
d1 = c1
