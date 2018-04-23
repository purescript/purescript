module Main where

import Prelude
import Effect.Console (log)
import Prim.Symbol (class Append)
import Type.Data.Symbol (SProxy(..), reflectSymbol)
import Type.Data.Symbol (append) as Symbol

sym :: SProxy ""
sym = SProxy

symA :: SProxy "A"
symA = SProxy

symB :: SProxy "B"
symB = SProxy

egAB :: SProxy "AB"
egAB = Symbol.append symA symB

egBA :: SProxy "BA"
egBA = Symbol.append symB symA

egA' :: SProxy "A"
egA' = Symbol.append sym (Symbol.append symA sym)

main = do
  let gotAB = reflectSymbol egAB == "AB"
      gotBA = reflectSymbol egBA == "BA"
      gotA' = reflectSymbol egA' == "A"
  when (not gotAB) $ log "Did not get AB"
  when (not gotBA) $ log "Did not get BA"
  when (not gotA') $ log "Did not get A"
  when (gotAB && gotBA && gotA') $ log "Done"
