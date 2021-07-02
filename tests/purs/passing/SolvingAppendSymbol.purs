module Main where

import Prelude
import Effect.Console (log)
import Prim.Symbol (class Append)
import Type.Proxy (Proxy(..))
import Type.Data.Symbol (append, reflectSymbol) as Symbol

sym :: Proxy ""
sym = Proxy

symA :: Proxy "A"
symA = Proxy

symB :: Proxy "B"
symB = Proxy

egAB :: Proxy "AB"
egAB = Symbol.append symA symB

egBA :: Proxy "BA"
egBA = Symbol.append symB symA

egA' :: Proxy "A"
egA' = Symbol.append sym (Symbol.append symA sym)

main = do
  let gotAB = Symbol.reflectSymbol egAB == "AB"
      gotBA = Symbol.reflectSymbol egBA == "BA"
      gotA' = Symbol.reflectSymbol egA' == "A"
  when (not gotAB) $ log "Did not get AB"
  when (not gotBA) $ log "Did not get BA"
  when (not gotA') $ log "Did not get A"
  when (gotAB && gotBA && gotA') $ log "Done"
