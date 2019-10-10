-- Cf. UnifyInTypeInstanceLookup.purs and failing/MissingEtaExpanson.purs
module Main where

import Effect.Console (log)

f :: Int -> Int
f i = f i

main = log "Done"
