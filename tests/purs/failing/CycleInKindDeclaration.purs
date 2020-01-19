-- @shouldFailWith CycleInKindDeclaration
module Main where

data Foo :: Bar -> Type
data Foo a = Foo

data Bar :: Foo -> Type
data Bar a = Bar
