module Main where

import Prelude
import Control.Monad.Eff.Console (log)

type A a = Array a

data Foo a = Foo (A a) | Bar

foo (Foo []) = Bar

main = log "Done"
