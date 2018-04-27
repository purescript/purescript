-- from #1244
module Main where

import Prelude
import Effect.Console (log)
import Bar (foo)

baz :: Int
baz = foo

main = log "Done"
