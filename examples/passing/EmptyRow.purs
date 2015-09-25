module Main where

import Prelude

data Foo r = Foo { | r }

test :: Foo ()
test = Foo {}

main = Control.Monad.Eff.Console.log "Done"
