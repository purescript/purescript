module Mod6 where

class B

instance b :: B

class C0 a where
  c0 :: a

class C1 a where
  c1 :: a

instance c0Int :: B => C0 Int where
  c0 = 0

instance c1Int :: C1 Int where
  c1 :: B => Int
  c1 = c0

d0 :: Int
d0 = c0

d1 :: Int
d1 = c1
