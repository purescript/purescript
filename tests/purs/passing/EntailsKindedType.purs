module Main where

import Prelude
import Effect
import Effect.Console

test x = show (x :: _ :: Type)

main = do
  when (show (unit :: Unit :: Type) == "unit") (log "Done")
  when (test unit == "unit") (log "Done")
