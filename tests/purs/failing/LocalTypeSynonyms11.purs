-- @shouldFailWith CycleInKindDeclaration
module Main where

foo :: Int
foo = result
  where
  type T1 :: T2
  type T1 = Type
  type T2 :: T1
  type T2 = Type
  result = 0
