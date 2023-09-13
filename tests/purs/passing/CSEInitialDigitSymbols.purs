module Main where

import Data.Symbol (class IsSymbol, reflectSymbol)
import Effect.Console (log)
import Type.Proxy (Proxy(..))

reflectSymbol' :: forall s. IsSymbol s => Proxy s -> String
reflectSymbol' = reflectSymbol

two = reflectSymbol (Proxy :: _ "2")
two2 = reflectSymbol' (Proxy :: _ "2")

twoThirty = reflectSymbol (Proxy :: _ "2:30")
twoThirty2 = reflectSymbol' (Proxy :: _ "2:30")

main = log "Done"
