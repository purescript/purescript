module Main where

import Effect.Console (log)

class Foo a where
  foo :: a

instance fooNumber :: Foo Number where
  foo :: Number
  foo = 0.0

main = log "Done"
