-- @shouldFailWith KindsDoNotUnify
module Main where

class To :: forall k. k -> k -> Constraint
class To a b | a -> b

instance to1 :: To Int "foo"
