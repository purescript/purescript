module Main where

import Prelude
import Partial.Unsafe (unsafeCrashWith)
import Effect.Console
import Effect

doIt :: Effect Boolean
doIt = pure true

set = do
  log "Testing..."
  case 42, 10 of
    42, 10 -> doIt
    _ , _  -> pure false

main = do
  b <- set
  case b of
    true -> log "Done"
    false -> unsafeCrashWith "Failed"
