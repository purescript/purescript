-- @shouldFailWith OverlappingNamesInLet
module Main where

foo :: Int
foo = result
  where
  type T = Int
  type T = String
  result = 0
