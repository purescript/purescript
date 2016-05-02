module A (module Prelude, foo) where
  import Prelude

  foo :: Number -> Number
  foo _ = 0.0

module Main where
  import Prelude
  import Control.Monad.Eff.Console (log, logShow)
  import A (foo)

  otherwise = false

  main = do
    logShow "1.0"
    log "Done"
