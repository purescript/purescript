-- @shouldFailWith KindsDoNotUnify
module Main where

type Syn (a :: * -> *) = String

val :: Syn Int
val = "bad"
