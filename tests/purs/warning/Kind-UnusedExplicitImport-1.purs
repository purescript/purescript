-- @shouldWarnWith UnusedExplicitImport
module Main where

import Prelude (Unit, unit, pure)
import Effect (Effect)
import Type.RowList (class ListToRow, RowList)

class A (@a :: RowList Type)

main :: Effect Unit
main = pure unit
