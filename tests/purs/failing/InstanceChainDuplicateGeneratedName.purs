-- @shouldFailWith DuplicatePartiallyGeneratedInstanceName
module Main where

import Effect.Console (log)

class Foo a
instance Foo x
else instance Foo x

main = log "Done"
