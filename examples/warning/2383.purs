-- | This specifically shouldn't warn about `x` being shadowed in `main`
-- | See https://github.com/purescript/purescript/issues/2383
module Main where

import Prelude

import Control.Monad.Eff (Eff)

main :: Eff () Unit
main = do
  x <- let x = pure unit in x
  pure unit
