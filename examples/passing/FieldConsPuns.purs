module Main where

import Prelude
import Control.Monad.Eff.Console (log, logShow)

greet { greeting, name } = log $ greeting <> ", " <> name <> "."

main = do
  greet { greeting, name }
  log "Done"
  where
  greeting = "Hello"
  name = "World"
