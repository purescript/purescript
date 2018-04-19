module Main where

import Prelude
import Effect.Console (log)
import Prim.Symbol (class Append)
import Type.Data.Symbol (SProxy(..), appendSymbol, reflectSymbol)

sym :: SProxy ""
sym = SProxy

symA :: SProxy "A"
symA = SProxy

symB :: SProxy "B"
symB = SProxy

egAB :: SProxy "AB"
egAB = appendSymbol symA symB

egBA :: SProxy "BA"
egBA = appendSymbol symB symA

egA' :: SProxy "A"
egA' = appendSymbol sym (appendSymbol symA sym)

main = do
  let gotAB = reflectSymbol egAB == "AB"
      gotBA = reflectSymbol egBA == "BA"
      gotA' = reflectSymbol egA' == "A"
  when (not gotAB) $ log "Did not get AB"
  when (not gotBA) $ log "Did not get BA"
  when (not gotA') $ log "Did not get A"
  when (gotAB && gotBA && gotA') $ log "Done"
