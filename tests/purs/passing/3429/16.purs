-- Cf. 17.purs and 20.purs
module Mod16 where

class B

instance b :: B

class C a where
  c0 :: Int -> a
  c1 :: Int -> a

instance cInt :: C Int where
  c0 i = c1 i
  c1 i = c0 i

d0 :: Int -> Int
d0 = c0

d1 :: Int -> Int
d1 = c1
