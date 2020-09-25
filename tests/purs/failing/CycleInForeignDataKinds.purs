-- @shouldFailWith CycleInKindDeclaration
module Main where

foreign import data Foo :: Bar
foreign import data Bar :: Foo
