-- Cf. 19.purs
module Mod17 where

class B

instance b :: B

class C a where
  c0 :: Int -> a
  c1 :: Int -> a

instance cInt :: C Int where
  c0 = \_ -> c0 0
  c1 = \_ -> c1 0

d0 :: Int -> Int
d0 = c0

d1 :: Int -> Int
d1 = c1
