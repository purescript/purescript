-- @shouldFailWith KindsDoNotUnify
module Main where

foo :: Int
foo = result
  where
  type Fst :: forall k. k -> k -> k
  type Fst a b = a
  type F = Fst Int "foo"
  result = 0
