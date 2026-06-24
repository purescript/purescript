-- @shouldFailWith DuplicateTypeArgument
module Main where

test :: Int
test = result
  where
  type DoubleArg a a = Int
  result = 0
