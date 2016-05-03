-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude
import Control.Monad.Eff.Console (log)

type Foo r = (x :: Number | r)
type Bar = { | Foo }

main = log "Done"
