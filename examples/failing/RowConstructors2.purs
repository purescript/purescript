-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude

type Foo r = (x :: Number | r)
type Bar = { | Foo }

main = Control.Monad.Eff.Console.log "Done"
