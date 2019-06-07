-- @shouldFailWith UnknownName
module Main where

import Control.Applicative (pure)
import Effect (Effect)

bar :: Effect Int
bar = foo \_ ->
  pure 10
