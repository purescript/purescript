-- @shouldFailWith UnknownName
module Main where

test = Foo.Bar.(-#-) 4 10
