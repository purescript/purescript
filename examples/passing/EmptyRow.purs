module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data Foo r = Foo { | r }

test :: Foo ()
test = Foo {}

main = log "Done"
