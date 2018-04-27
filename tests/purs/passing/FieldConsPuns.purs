module Main where

import Prelude
import Effect.Console (log, logShow)

greet { greeting, name } = log $ greeting <> ", " <> name <> "."

main = do
  greet { greeting, name }
  log "Done"
  where
  greeting = "Hello"
  name = "World"
