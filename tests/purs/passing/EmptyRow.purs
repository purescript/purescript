module Main where

import Prelude
import Effect.Console (log)

data Foo r = Foo { | r }

test :: Foo ()
test = Foo {}

main = log "Done"
