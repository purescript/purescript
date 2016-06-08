-- @shouldFailWith KindsDoNotUnify
module Main where

import Control.Monad.Eff.Console (log)

type Foo = { x :: Number }
type Bar = { | Foo }

main = log "Done"
