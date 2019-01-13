-- @shouldFailWith HoleInferredType
module Main where

import Prelude
import Effect (Effect)

main :: Effect ?ummm
main = pure unit
