-- from #1244
module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Bar (foo)

baz :: Int
baz = foo

main = log "Done"
