-- Cf. 20.purs
module Mod21 where

class C0 a where
  c0 :: Int -> a

class C1 a where
  c1 :: Int -> a

instance c0Int :: C0 Int where
  c0 = c1

instance c1Int :: C1 Int where
  c1 _ = c0 0

d0 :: Int -> Int
d0 = c0

d1 :: Int -> Int
d1 = c1
