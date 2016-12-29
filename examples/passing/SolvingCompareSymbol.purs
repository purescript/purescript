module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Type.Data.Symbol (SProxy(..), class CompareSymbol, compareSymbol)
import Type.Data.Ordering (OProxy(..), kind Ordering, LT, EQ, GT, reflectOrdering)

symA :: SProxy "A"
symA = SProxy

symB :: SProxy "B"
symB = SProxy

egLT :: OProxy LT
egLT = compareSymbol symA symB

egEQ :: OProxy EQ
egEQ = compareSymbol symA symA

egGT :: OProxy GT
egGT = compareSymbol symB symA

main = do
  let gotLT = reflectOrdering egLT == LT
      gotEQ = reflectOrdering egEQ == EQ
      gotGT = reflectOrdering egGT == GT
  when (not gotLT) $ log "Did not get LT"
  when (not gotEQ) $ log "Did not get EQ"
  when (not gotGT) $ log "Did not get GT"
  when (gotLT && gotEQ && gotGT) $ log "Done"
