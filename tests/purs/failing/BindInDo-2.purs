-- @shouldFailWith CannotUseBindWithDo
module Main where

import Prelude

foo = do
  let bind = 42
  x <- [4, 5, 6]
  pure x
