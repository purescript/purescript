module Main where

import Prelude
import Effect.Console

data Foo = Foo { id :: forall a. a -> a }

foo :: Foo -> Number
foo (Foo { id: f }) = f 0.0

main = log "Done"
