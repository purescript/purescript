-- @shouldFailWith CycleInDictDeclaration
-- @shouldFailWith CycleInDictDeclaration
module Main where

class B

instance b :: B

class C0 a where
  c0 :: a

class C1 a where
  c1 :: a

instance c0Int :: B => C0 Int where
  c0 :: B => Int
  c0 = c1

instance c1Int :: B => C1 Int where
  c1 :: B => Int
  c1 = c0
