-- @shouldFailWith DeriveClauseArityError
module Main where

class MyMultiParam a b where
  doSomething :: a -> b

data Foo = Foo
  derive (MyMultiParam)
