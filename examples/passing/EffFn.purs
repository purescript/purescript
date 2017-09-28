module Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Uncurried (EffFn3, mkEffFn7, runEffFn3, runEffFn7)
import Test.Assert (assert)

testBothWays = do
  res <- (runEffFn7 $ mkEffFn7 \x1 x2 x3 x4 x5 x6 x7 -> pure 42) 1 2 3 4 5 6 7
  assert $ res == 42

foreign import add3 :: forall eff. EffFn3 eff String String String String

testRunFn = do
  str <- runEffFn3 add3 "a" "b" "c"
  assert $ str == "abc"

main = do
  testBothWays
  testRunFn
  log "Done"
