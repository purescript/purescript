-- @shouldWarnWith HidingImport
-- @shouldWarnWith HidingImport
module Main where

import Prelude hiding (one)
import Effect hiding (untilE)

main :: Effect Unit
main = pure unit
