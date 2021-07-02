-- @shouldFailWith KindsDoNotUnify
module Main where

data Tricky :: forall r. {a :: Int | r} -> {b :: Int | r} -> Type
data Tricky x y = Tricky

type MkTricky x = Tricky x x
