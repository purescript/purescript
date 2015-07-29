module Main where

import Prelude

class Foo a where
  foo :: a -> String

instance fooString :: Foo String where
  foo = go
    where
    go :: String -> String
    go s = s

main = Control.Monad.Eff.Console.log "Done"
