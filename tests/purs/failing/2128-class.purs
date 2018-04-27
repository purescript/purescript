-- @shouldFailWith ErrorParsingModule
module Main where

class Foo a where
  foo :: a -> !!!
