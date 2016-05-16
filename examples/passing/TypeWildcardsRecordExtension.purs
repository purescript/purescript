module Main where

import Prelude
import Control.Monad.Eff.Console (log)

foo :: forall a. {b :: Number | a} -> {b :: Number | _}
foo f = f

main = log "Done"
