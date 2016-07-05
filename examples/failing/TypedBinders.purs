-- @shouldFailWith ErrorParsingModule
module Main where

import Control.Monad.Eff.Console (log)

test = (\f :: Int -> Int -> f 10) id

main = do
  let t1 = test
  log "Done"
