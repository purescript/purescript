module Main where

import Prelude
import Control.Monad.Eff.Console
import Control.Monad.Eff

doIt :: forall eff. Eff eff Boolean
doIt = return true

set = do
  log "Testing..."
  case 42, 10 of
    42, 10 -> doIt
    _ , _  -> return false

main = do
  b <- set
  case b of
    true -> log "Done"
