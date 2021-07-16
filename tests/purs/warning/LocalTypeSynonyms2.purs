-- @shouldWarnWith ShadowedTypeName
module Main where

type Foo = Int

test :: Foo
test = if truth then 0 else 1
  where
  type Foo = Boolean
  truth :: Foo
  truth = true
