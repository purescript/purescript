module Main where

import Prelude
import Effect.Console (log)

main = do
  let
    x = { a: 42, b: "foo" }
    { a, b } = x { a = 43 }
  log "Done"
