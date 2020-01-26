-- @shouldWarnWith UnusedImport
module Main where

import Prelude (Unit, unit, pure)
import Effect (Effect)
import Type.RowList (kind RowList)

main :: Effect Unit
main = pure unit
