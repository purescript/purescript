module Main where

import Prelude

import Effect.Console (log)
import Control.Monad.ST as ST
import Control.Monad.ST.Uncurried (STFn3, mkSTFn7, runSTFn3, runSTFn7)
import Test.Assert (assert)

testBothWays = do
  let res = ST.run ((runSTFn7 $ mkSTFn7 \x1 x2 x3 x4 x5 x6 x7 -> pure 42) 1 2 3 4 5 6 7)
  assert $ res == 42

foreign import add3 :: STFn3 String String String String

testRunFn = do
  let str = ST.run (runSTFn3 add3 "a" "b" "c")
  assert $ str == "abc"

main = do
  testBothWays
  testRunFn
  log "Done"
