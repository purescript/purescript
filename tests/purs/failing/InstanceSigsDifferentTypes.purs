-- @shouldFailWith TypesDoNotUnify

module Main where

class Foo a where
  foo :: a

instance fooNumber :: Foo Number where
  foo :: Int
  foo = 0.0
