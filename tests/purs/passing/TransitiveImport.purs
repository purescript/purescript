module Main where

  import Prelude
  import Middle
  import Effect.Console

  main = do
    logShow (middle unit)
    log "Done"
