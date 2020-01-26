-- @shouldWarnWith UnusedExplicitImport
module Main where

import Prelude (Unit, unit, pure)
import Effect (Effect)
import Type.RowList (RLProxy, kind RowList)

class A (a :: RowList)

main :: Effect Unit
main = pure unit
