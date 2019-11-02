-- @shouldFailWith CaseBinderLengthDiffers
module Main where

test = case 1, 2 of
  1, 2, 3 -> 42
  _, _    -> 43
