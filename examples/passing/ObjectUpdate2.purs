module Main where

import Prelude

type X r = { | r }

x :: X (baz :: String)
x = { baz: "baz" }

blah :: forall r. X r -> X r
blah x = x

test = blah x
  { baz = "blah"
  }

main = Control.Monad.Eff.Console.log "Done"
