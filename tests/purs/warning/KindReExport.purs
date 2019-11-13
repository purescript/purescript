-- | This test is to ensure that we do not get an incorrect 'unused kind'
-- | warning. See #3744
module Main (main, module X) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Prim.Ordering (kind Ordering) as X

main :: Effect Unit
main = log "Done"
