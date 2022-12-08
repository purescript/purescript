-- @shouldWarnWith ShadowedName
module Main where

test :: Int
test = result
  where
  type Foo = Int
  result :: Foo
  result = if truth then 0 else 1
    where
    type Foo = Boolean
    truth :: Foo
    truth = true
