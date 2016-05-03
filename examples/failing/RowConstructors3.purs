-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude
import Control.Monad.Eff.Console (log)

type Foo = { x :: Number }
type Bar = { | Foo }

main = log "Done"
