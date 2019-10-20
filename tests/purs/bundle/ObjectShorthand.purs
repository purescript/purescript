-- See issue #3741
module Main (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert)

main :: Effect Unit
main = do
  assert (bar.foo == "foo")
  log "Done"

foreign import bar :: { foo :: String }
