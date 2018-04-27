module Main where

import Prelude
import Effect.Console (log)

shout = log <<< (_ <> "!") <<< show

main = do
  shout "Test"
  log "Done"
