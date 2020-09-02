module Main (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)

main :: Effect Unit
main = do
  assert (bar == 1)
  log "Done"

foreign import bar :: Int
