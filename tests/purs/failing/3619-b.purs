-- @shouldFailWith AmbiguousTypeVariables
module Main where

import Control.Applicative (pure)

bar = ?a \_ ->
  pure 10
