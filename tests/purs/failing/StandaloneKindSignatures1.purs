-- @shouldFailWith KindsDoNotUnify
module Main where

data Pair :: forall k. k -> k -> Type
data Pair a b = Pair

test = Pair :: Pair Int "foo"
