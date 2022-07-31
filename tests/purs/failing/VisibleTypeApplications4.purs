-- @shouldFailWith AmbiguousTypeVariables
module Main where

class Foo foo where
  foo :: String

foo' = foo @_
