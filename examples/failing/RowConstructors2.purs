-- @shouldFailWith KindsDoNotUnify
module Main where

import Control.Monad.Eff.Console (log)

type Foo r = (x :: Number | r)
type Bar = { | Foo }

main = log "Done"
