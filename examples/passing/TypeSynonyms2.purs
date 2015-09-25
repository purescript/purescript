module Main where

import Prelude

class Foo a where
  foo :: a -> String

type Bar = String

instance fooBar :: Foo Bar where
  foo s = s

main = Control.Monad.Eff.Console.log "Done"
