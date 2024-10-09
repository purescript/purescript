-- @shouldWarnWith UnusedName
-- @shouldWarnWith UnusedName
module Main where

import Prelude

import Effect (Effect)

test1 :: Int
test1 = 0
  where
  type Foo = Boolean

test2 :: Effect Int
test2 = do
  let type Foo = Boolean
  pure 0

test3 :: Int
test3 = if truth then 0 else 1
  where
  type Foo = Boolean
  truth = (true :: Foo)

test4 :: Effect Int
test4 = do
  let type Foo = Boolean
      truth :: Foo
      truth = true
  pure $ if truth then 0 else 1
