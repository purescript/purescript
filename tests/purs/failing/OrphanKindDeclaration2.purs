-- @shouldFailWith OrphanKindDeclaration
module Main where

type Foo :: Type
data Foo = Foo Int
