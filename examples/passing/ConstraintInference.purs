module Main where

import Prelude
import Control.Monad.Eff.Console (log)

shout = log <<< (_ <> "!") <<< show

main = do
  shout "Test"
  log "Done"
