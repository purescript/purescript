-- @shouldFailWith ErrorParsingModule 
module Main where

import Prelude

test = (\f :: Int -> Int -> f 10) id

main = do
  let t1 = test
  Control.Monad.Eff.Console.log "Done"