-- @shouldFailWith OrphanTypeDeclaration

module Main where

class Foo a where
  foo :: a

instance fooNumber :: Foo Number where
  bar :: Int
  foo = 0.0
