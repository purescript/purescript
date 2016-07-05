module Main where

import Prelude
import Control.Monad.Eff.Console (logShow, log)

main = do
  logShow (const unit $ "Hello world")
  log "Done"
