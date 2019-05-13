-- @shouldFailWith CycleInDictDeclaration
module Main where

class B0

instance b0 :: B0

class B1 a

instance b1 :: B1 a

class C a where
  c0 :: a
  c1 :: a

instance cInt :: (B0, B1 Int) => C Int where
  c0 = 0
  c1 = c0
