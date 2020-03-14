-- @shouldFailWith KindsDoNotUnify
module Main where

type Fst :: forall k. k -> k -> k
type Fst a b = a

type F = Fst Int "foo"
