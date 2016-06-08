-- @shouldFailWith KindsDoNotUnify
module Main where

import Control.Monad.Eff.Console (log)

data Foo = Bar
type Baz = { | Foo }

main = log "Done"
