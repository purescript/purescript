module Main where

import Effect.Console (log)

class Foo a where
  foo :: a

instance fooArray :: forall a. (Foo a) => Foo (Array a) where
  foo :: Array a
  foo = []

main = log "Done"
