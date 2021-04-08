module Main where

import Effect.Console (log)

class A :: Constraint
class A

class B :: Constraint
class A <= B

main = log "Done"
