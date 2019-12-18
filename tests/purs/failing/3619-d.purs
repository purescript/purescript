-- @shouldFailWith NoInstanceFound
module Main where

import Control.Applicative (pure)
import Effect (Effect)

bar :: Effect Int
bar = ?a \_ ->
  pure 10
