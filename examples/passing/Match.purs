module Main where

import Prelude

data Foo a = Foo

foo = \f -> case f of Foo -> "foo"

main = Control.Monad.Eff.Console.log "Done"
