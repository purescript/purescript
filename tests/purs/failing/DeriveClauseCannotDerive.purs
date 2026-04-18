-- @shouldFailWith CannotDerive
module Main where

class MyClass a

data Foo a = Foo a
  derive (MyClass)
