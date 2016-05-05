module Main where

import Prelude
import Partial.Unsafe (unsafeCrashWith)
import Control.Monad.Eff.Console
import Control.Monad.Eff

doIt :: forall eff. Eff eff Boolean
doIt = pure true

set = do
  log "Testing..."
  case 0 of
    0 -> doIt
    _ -> pure false

main = do
  b <- set
  case b of
    true -> log "Done"
    false -> unsafeCrashWith "Failed"
