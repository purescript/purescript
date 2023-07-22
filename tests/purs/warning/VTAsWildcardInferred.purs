-- @shouldWarnWith WildcardInferredType
module Main where

identity :: forall y. y -> y
identity y = y

f :: forall @a. a -> a
f = identity

x :: { x :: Int }
x = f @{ x :: _ } { x: 42 }
