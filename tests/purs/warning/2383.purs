-- | This specifically shouldn't warn about `x` being shadowed in `main`
-- | See https://github.com/purescript/purescript/issues/2383
module Main where

import Prelude

import Effect (Effect)

main :: Effect Unit
main = do
  x <- let x = pure unit in x
  pure unit
