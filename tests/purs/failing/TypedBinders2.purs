-- @shouldFailWith TypeConstructorsDoNotUnify
module Main where

import Prelude
import Effect.Console (log)

main = do
  s :: String <- log "Foo"
  log "Done"
