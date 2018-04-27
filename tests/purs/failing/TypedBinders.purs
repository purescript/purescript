-- @shouldFailWith ErrorParsingModule
module Main where

import Effect.Console (log)

test = (\f :: Int -> Int -> f 10) identity

main = do
  let t1 = test
  log "Done"
