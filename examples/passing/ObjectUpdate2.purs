module Main where

import Prelude
import Control.Monad.Eff.Console (log)

type X r = { | r }

x :: X (baz :: String)
x = { baz: "baz" }

blah :: forall r. X r -> X r
blah x = x

test = blah x
  { baz = "blah"
  }

main = log "Done"
