-- @shouldFailWith ErrorParsingModule
module Main where

class Foo a where
  foo :: a

instance fooInt :: Foo Int where
  foo = !!!
