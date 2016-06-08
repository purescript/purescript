module Main where

import Prelude
import Control.Monad.Eff.Console (log)

instance fooNumber :: Foo Number where
  foo = 0.0

class Foo a where
  foo :: a

main = log "Done"
