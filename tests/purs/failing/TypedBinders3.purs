-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude
import Effect.Console (log)

test = case 1 of
  (0 :: String) -> true
  _ -> false

main = do
  let t = test
  log "Done"
