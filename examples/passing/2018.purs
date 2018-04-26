module Main where

import Prelude
import A (foo)
import B (Foo(..))
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  let tmp = foo X
  log "Done"
