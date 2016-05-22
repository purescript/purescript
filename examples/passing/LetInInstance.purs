module Main where

import Prelude
import Control.Monad.Eff.Console (log)

class Foo a where
  foo :: a -> String

instance fooString :: Foo String where
  foo = go
    where
    go :: String -> String
    go s = s

main = log "Done"
