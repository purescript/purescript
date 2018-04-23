-- @shouldWarnWith HidingImport
-- @shouldWarnWith HidingImport
module Main where

import Prelude hiding (one)
import Effect hiding (Effect)

main :: Effect Unit
main = pure unit
