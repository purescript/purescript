module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Data.Array

test = runSTArray (do
  a <- newSTArray 2 0
  pokeSTArray a 0 1
  pokeSTArray a 1 2
  return a)

fromTo' lo hi _ arr | lo > hi = return arr
fromTo' lo hi i arr = do
  pokeSTArray arr i lo
  fromTo' (lo + 1) hi (i + 1) arr

fromTo lo hi = runSTArray (do
  arr <- newSTArray (hi - lo + 1) 0
  fromTo' lo hi 0 arr)

main = Debug.Trace.print $ runPure (fromTo 10 20)
