module Main where

import Prelude
import Control.Monad.Eff.Console (log)

foo :: forall t. Warn "Example" => t -> t
foo x = x

main = when (foo 42 == 42) $ log "Done"
