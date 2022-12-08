-- @shouldFailWith CycleInTypeSynonym
module Main where

foo :: Int
foo = result
  where
  type T1 = Array T1
  result = 0
