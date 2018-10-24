module Main where

import Prelude (Unit)
import Unsafe.Coerce (unsafeCoerce)
import Effect (Effect)
import Effect.Console (log)

x :: Number
x = unsafeCoerce 1

y :: Number
y = case unsafeCoerce 1 of
  z -> unsafeCoerce z

main :: Effect Unit
main = log "Done"
