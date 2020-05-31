-- @shouldFailWith KindsDoNotUnify
module Main where

data Pair :: forall k. k -> k -> Type
data Pair a b = Pair

newtype Pair' :: forall k1 k2. k1 -> k2 -> Type
newtype Pair' a b = Pair' (Pair a b)
