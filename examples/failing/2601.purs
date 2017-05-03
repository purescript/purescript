-- @shouldFailWith KindsDoNotUnify
module Main where

type Syn (a :: Type -> Type) = String

val :: Syn Int
val = "bad"
