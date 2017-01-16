module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.Assert

data V

derive instance eqV :: Eq V

derive instance ordV :: Ord V

type MyString = String

data X = X Int | Y MyString

derive instance eqX :: Eq X

derive instance ordX :: Ord X

newtype Z = Z { left :: X, right :: X }

derive instance eqZ :: Eq Z

main = do
  assert $ X 0 == X 0
  assert $ X 0 /= X 1
  assert $ Y "Foo" == Y "Foo"
  assert $ Y "Foo" /= Y "Bar"
  assert $ X 0 < X 1
  assert $ X 0 < Y "Foo"
  assert $ Y "Bar" < Y "Baz"
  assert $ z == z
  log "Done"
  where
  z = Z { left: X 0, right: Y "Foo" }
