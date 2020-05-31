-- @shouldWarnWith UnusedExplicitImport
module Main where

import Prelude (Unit, unit, pure)
import Effect (Effect)
import Type.RowList (RLProxy, RowList)

f :: forall l. RLProxy l -> Int
f _ = 0

main :: Effect Unit
main = pure unit
