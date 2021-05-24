-- @shouldFailWith ErrorParsingModule
module Main where

import Effect.Console (log)

class Foo a
-- the "::" separator between the name and class name
-- needs to be added.
instance instanceName Foo x
-- else instance Foo x

main = log "Done"
