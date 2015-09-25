-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude

type Foo = { x :: Number }
type Bar = { | Foo }

main = Control.Monad.Eff.Console.log "Done"
