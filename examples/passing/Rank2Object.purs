module Main where

import Prelude
import Control.Monad.Eff.Console

data Foo = Foo { id :: forall a. a -> a }

foo :: Foo -> Number
foo (Foo { id = f }) = f 0.0

main = log "Done"
