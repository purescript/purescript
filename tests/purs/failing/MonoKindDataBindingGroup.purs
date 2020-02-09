-- @shouldFailWith KindsDoNotUnify
module Main where

data F (a :: Type -> Type) = F
data A a = A (B a)
type B a = F A

type X = A "bad"
