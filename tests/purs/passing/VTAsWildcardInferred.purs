-- See https://github.com/purescript/purescript/issues/4487
module Main where

import Prelude
import Effect.Console (log)

f :: forall @a. a -> a
f = identity

x :: { x :: Int }
x = f @{ x :: _ } { x: 42 }

main = log "Done"
