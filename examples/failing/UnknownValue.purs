module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Debug.Trace

test = runSTArray (do
  a <- newSTArray 2 0
  pokeSTArray a 0 1
  pokeSTArray a 1 2
  return a)

fromTo lo hi = runSTArray (do
  arr <- newSTArray (hi - lo + 1) 0
  (let
    go lo hi _ arr | lo > hi = return arr
    go lo hi i arr = do
      pokeSTArray arrr i lo
      go (lo + 1) hi (i + 1) arr
   in go lo hi 0 arr))

main = do
  let t1 = runPure (fromTo 10 20)
  trace "Done"
