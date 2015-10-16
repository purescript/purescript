-- @shouldFailWith DuplicateValueDeclaration
module Main where

data X = X | Y

class Foo a where
  foo :: a -> a
  bar :: a

instance fooX :: Foo X where
  foo X = X
  bar = X
  foo Y = Y
