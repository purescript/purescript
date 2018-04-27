-- @shouldFailWith KindsDoNotUnify
module Main where

import Effect.Console (log)

data Foo = Bar
type Baz = { | Foo }

main = log "Done"
