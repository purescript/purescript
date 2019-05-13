-- @shouldFailWith CycleInDictDeclaration
module Main where

class C a where
  c0 :: a -> a
  c1 :: a -> a
  c2 :: a -> a
  c3 :: a -> a
  c4 :: a -> a

instance cInt :: C Int where
  c0 _ = 0
  c1 = c0
  c2 = c0
  c3 = c0
  c4 = c0
