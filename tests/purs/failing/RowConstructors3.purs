-- @shouldFailWith KindsDoNotUnify
module Main where

import Effect.Console (log)

type Foo = { x :: Number }
type Bar = { | Foo }

main = log "Done"
