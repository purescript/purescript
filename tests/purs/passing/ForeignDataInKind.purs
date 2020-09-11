module Main where

import Prelude
import Effect.Console (log)

foreign import data A :: Type
data B (x :: A)

main = log "Done"
