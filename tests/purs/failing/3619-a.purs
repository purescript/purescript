-- @shouldFailWith AmbiguousTypeVariables
module Main where

import Control.Applicative (pure)

bar = foo \_ ->
  pure 10
