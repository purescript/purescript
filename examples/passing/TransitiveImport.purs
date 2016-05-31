module Main where

  import Prelude
  import Middle
  import Control.Monad.Eff.Console

  main = do
    logShow (middle unit)
    log "Done"
