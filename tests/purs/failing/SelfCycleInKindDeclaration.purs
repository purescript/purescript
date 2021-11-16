-- @shouldFailWith CycleInKindDeclaration
module Main where

data Foo :: Foo -> Type
data Foo a = Foo
