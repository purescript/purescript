module Main where

import Control.Monad.Eff.Console (log)

class Foo (a :: Symbol)

instance fooX :: Foo "x"

main = log "Done"
