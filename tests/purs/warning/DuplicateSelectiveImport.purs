-- @shouldWarnWith DuplicateSelectiveImport
module Main where

import Prelude (Unit, unit)
import Prelude (pure)

import Effect (Effect)

main :: Effect Unit
main = pure unit
