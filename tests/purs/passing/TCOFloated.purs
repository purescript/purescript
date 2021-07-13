module Main where

import Prelude
import Effect.Console (log)

main = log (looper { foo: 100000 })

-- The Ord instance for { foo :: Int } will be floated to an IIFE wrapping the
-- function. This test verifies that TCO happens anyway.
looper :: { foo :: Int } -> String
looper x = if x <= { foo: 0 } then "Done" else looper { foo: x.foo - 1 }
