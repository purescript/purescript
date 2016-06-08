-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude
import Control.Monad.Eff.Console (log)

main = do
  s :: String <- log "Foo"
  log "Done"
