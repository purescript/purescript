-- @shouldFailWith TypesDoNotUnify

module Main where

class Eq a where
  eq :: a -> a -> Boolean

instance eqNumber :: Eq Number where
  eq :: forall x y. x -> y -> Boolean
  eq _ _ = true
