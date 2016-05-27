module Main where

import Control.Monad.Eff.Console (log, logShow)
import A

main = do
  logShow (show 1.0)
  log "Done"
