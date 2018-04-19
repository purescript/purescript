-- @shouldFailWith KindsDoNotUnify
module Main where

import Effect.Console (log)

type Foo r = (x :: Number | r)
type Bar = { | Foo }

main = log "Done"
