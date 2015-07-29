module Main where

import Prelude

instance fooNumber :: Foo Number where
  foo = 0.0

class Foo a where
  foo :: a

main = Control.Monad.Eff.Console.log "Done"
