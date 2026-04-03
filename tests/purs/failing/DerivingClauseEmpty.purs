-- @shouldFailWith ErrorParsingModule
module Main where

data Foo = Foo
  derive ()
