module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

data X = Control_Bind

main :: Effect Unit
main = do
  pure unit
  log "Done"
