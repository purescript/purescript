-- @shouldFailWith CycleInDictDeclaration
module Main where

class C a where
  c0 :: a
  c1 :: a
  c2 :: a
  c3 :: a
  c4 :: a

instance cInt :: C Int where
  c0 = 0
  c1 = c0
  c2 = c0
  c3 = c0
  c4 = c0
