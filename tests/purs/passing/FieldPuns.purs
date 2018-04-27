module Main where

import Prelude
import Effect.Console

greet { greeting, name } = log $ greeting <> ", " <> name <> "."

main = do
  greet { greeting: "Hello", name: "World" }
  log "Done"
