-- @shouldFailWith UnknownName
module Main where

import Data.Maybe (Maybe(..))
import Prelude (pure)

x = do
  x <- Just 1
  pure x
