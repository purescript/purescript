-- @shouldWarnWith UnusedExplicitImport
module Main where

import Prelude (Unit, unit, pure)
import Effect (Effect)
import Type.RowList (class ListToRow, RowList)
import Type.Proxy (Proxy)

f :: forall l r. ListToRow l r => Proxy l -> Int
f _ = 0

main :: Effect Unit
main = pure unit
