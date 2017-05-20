-- @shouldFailWith ErrorParsingModule
module Main where

class Foo a where
  foo :: a

class Baz b where
  baz :: b

instance bazFoo :: (Baz _) => Foo b where
  foo = baz
