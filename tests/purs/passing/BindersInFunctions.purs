module Main where

import Prelude
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert')
import Effect (Effect)
import Effect.Console (log)

snd :: forall a. Partial => Array a -> a
snd = \[_, y] -> y

main :: Effect _
main = do
  let ts = unsafePartial (snd [1.0, 2.0])
  assert' "Incorrect result from 'snd'." (ts == 2.0)
  log "Done"
