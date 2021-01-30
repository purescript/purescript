-- @shouldFailWith UnknownName
module Main where

test = 4 Foo.Bar.-#- 10
