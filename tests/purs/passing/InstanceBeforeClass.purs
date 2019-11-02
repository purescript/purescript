module Main where

import Prelude
import Effect.Console (log)

instance fooNumber :: Foo Number where
  foo = 0.0

class Foo a where
  foo :: a

main = log "Done"
