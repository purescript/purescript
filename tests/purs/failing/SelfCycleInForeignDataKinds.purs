-- @shouldFailWith CycleInKindDeclaration
module Main where

foreign import data Foo :: Foo
