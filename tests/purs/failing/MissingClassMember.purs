-- @shouldFailWith MissingClassMember
module Main where

class A a where
  a :: a -> String
  b :: a -> Number
  c :: forall f. a -> f a

instance aString :: A String where
  a s = s
