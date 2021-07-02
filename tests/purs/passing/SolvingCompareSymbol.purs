module Main where

import Prelude
import Effect.Console (log)
import Prim.Symbol (class Compare)
import Prim.Ordering (kind Ordering, LT, EQ, GT)
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (compare) as Symbol
import Type.Data.Ordering (reflectOrdering)

symA :: Proxy "A"
symA = Proxy

symB :: Proxy "B"
symB = Proxy

egLT :: Proxy LT
egLT = Symbol.compare symA symB

egEQ :: Proxy EQ
egEQ = Symbol.compare symA symA

egGT :: Proxy GT
egGT = Symbol.compare symB symA

main = do
  let gotLT = reflectOrdering egLT == LT
      gotEQ = reflectOrdering egEQ == EQ
      gotGT = reflectOrdering egGT == GT
  when (not gotLT) $ log "Did not get LT"
  when (not gotEQ) $ log "Did not get EQ"
  when (not gotGT) $ log "Did not get GT"
  when (gotLT && gotEQ && gotGT) $ log "Done"
