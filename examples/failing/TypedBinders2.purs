-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude

main = do
  s :: String <- Control.Monad.Eff.Console.log "Foo"
  Control.Monad.Eff.Console.log "Done"

