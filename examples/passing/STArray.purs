module Main where

import Prelude
import Eff
import ST
import Arrays

test = runSTArray (do
  a <- newSTArray 2 0
  pokeSTArray a 0 1
  pokeSTArray a 1 2
  return a)

main = Trace.print $ runPure test
