module Main where

import Effect (Effect)
import Effect.Console (log)
import Prelude

pu :: forall i. i -> Effect Unit
pu _ = pure unit

type C i = { pu :: i -> Effect Unit }

sampleC :: C Unit
sampleC = { pu: pu }

newtype Identity a = Id a

sampleIdC :: Identity (C Unit)
sampleIdC = Id { pu : pu }

main = log "Done"
