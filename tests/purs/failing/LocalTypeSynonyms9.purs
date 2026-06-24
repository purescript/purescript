-- @shouldFailWith KindsDoNotUnify
module Main where

test :: Int
test = 0
  where
  type ShouldNotGeneralize a = a
  type A = ShouldNotGeneralize Int
  type B = ShouldNotGeneralize "symbol"
