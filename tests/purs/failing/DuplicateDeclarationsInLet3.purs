-- @shouldFailWith OverlappingNamesInLet
-- @shouldFailWith OverlappingNamesInLet
module Main where

-- Should see separate errors for `a` and `interrupted`
foo = interrupter + a
  where
  a = 0
  a :: Int
  a = 0

  interrupted true = 1

  interrupter = 2

  interrupted false = 3
