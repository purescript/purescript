module Main where

import Prelude

import Effect.Console (log)
import Test.Assert (assert)

import Foo as Foo.Bar

main = do
  assert $ 4 Foo.Bar.-#- 10 == 33
  assert $ Foo.Bar.(-#-) 4 10 == 33
  log "Done"
