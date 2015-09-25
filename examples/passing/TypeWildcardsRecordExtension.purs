module Main where

import Prelude

foo :: forall a. {b :: Number | a} -> {b :: Number | _}
foo f = f

main = Control.Monad.Eff.Console.log "Done"
