module Main where

  import Control.Monad.Eff.Console
  import A
  import B
  import C
  import Prelude

  main = do
    logShow (show 1.0)
    log "Done"
