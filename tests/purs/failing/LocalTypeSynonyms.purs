-- @shouldFailWith CycleInTypeSynonym
module Main where

foo :: Int
foo = result
  where
  type T1 = Array T2
  type T2 = T1
  result = 0
