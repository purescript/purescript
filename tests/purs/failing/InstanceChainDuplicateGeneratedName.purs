-- @shouldFailWith DuplicatePartiallyGeneratedInstanceName
module Main where

import Effect.Console (log)

class Foo a
instance Foo x
else instance Foo x

class Foo2 a b

instance Foo2 Int String
else instance Foo2 Int String

main = log "Done"
