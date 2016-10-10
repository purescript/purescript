module Main where

import Control.Monad.Eff.Console (log)

newtype Bar' = Bar' Int

data Foo' = Foo' Bar'

data Baz'' = Baz''

main = log "Done"
