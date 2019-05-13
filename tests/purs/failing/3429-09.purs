-- @shouldFailWith CycleInDictDeclaration
module Main where

class B

instance b :: B

class C a where
  c0 :: Int -> a
  c1 :: Int -> a

instance cInt :: C Int where
  c0 _ = 0
  c1 :: B => Int -> Int
  c1 = c0

c :: Int -> Int
c = c0
