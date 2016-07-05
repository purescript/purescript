module Main where

import Prelude
import Control.Monad.Eff.Console (log)

foo = \x -> case x of
  y@{ foo: "Foo" } -> y
  y -> y

main = log "Done"
