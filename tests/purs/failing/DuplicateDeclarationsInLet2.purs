-- @shouldFailWith OverlappingNamesInLet
module Main where

foo = interrupted
  where
  interrupted true = 1

  interrupter = 2

  interrupted false = 3
