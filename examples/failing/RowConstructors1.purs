-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data Foo = Bar
type Baz = { | Foo }

main = log "Done"
