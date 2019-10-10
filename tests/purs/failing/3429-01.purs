-- @shouldFailWith CycleInDictDeclaration
module Main where

class B

instance b :: B

class C a where
  c0 :: a
  c1 :: a

instance cInt :: B => C Int where
  c0 = 0
  c1 = c0
