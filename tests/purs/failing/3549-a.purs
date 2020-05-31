-- @shouldFailWith UnknownName
module Main where

import Effect.Console (log)

identity :: forall (a :: Typ) . a -> a
identity x = x

main = log "Done"

