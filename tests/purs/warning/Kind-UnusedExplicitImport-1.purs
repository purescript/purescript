-- @shouldWarnWith UnusedExplicitImport
module Main where

import Prelude (Unit, unit, pure)
import Effect (Effect)
import Type.RowList (RLProxy, RowList)

class A (a :: RowList Type)

main :: Effect Unit
main = pure unit
