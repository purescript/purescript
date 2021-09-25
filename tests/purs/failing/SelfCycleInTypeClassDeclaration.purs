-- @shouldFailWith CycleInTypeClassDeclaration
module Main where

class (Foo a) <= Foo a
