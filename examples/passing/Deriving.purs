module Main where

import Prelude
import Test.Assert

data X = X Int | Y String

derive instance eqX :: Eq X

derive instance ordX :: Ord X

main = do
  assert $ X 0 == X 0
  assert $ X 0 /= X 1
  assert $ Y "Foo" == Y "Foo"
  assert $ Y "Foo" /= Y "Bar"
  assert $ X 0 < X 1
  assert $ X 0 < Y "Foo"
  assert $ Y "Bar" < Y "Baz"
