module Main where

import Prelude

import Effect.Console (log)
import Effect.Uncurried (EffectFn3, mkEffectFn7, runEffectFn3, runEffectFn7)
import Test.Assert (assert)

testBothWays = do
  res <- (runEffectFn7 $ mkEffectFn7 \x1 x2 x3 x4 x5 x6 x7 -> pure 42) 1 2 3 4 5 6 7
  assert $ res == 42

foreign import add3 :: EffectFn3 String String String String

testRunFn = do
  str <- runEffectFn3 add3 "a" "b" "c"
  assert $ str == "abc"

main = do
  testBothWays
  testRunFn
  log "Done"
