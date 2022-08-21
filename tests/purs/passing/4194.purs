module Main where

import Effect.Console (log)

data Identity a
data Maybe a

class ErrorSemigroup o m w | w -> o m, o m -> w

instance ErrorSemigroup (Identity o) (Identity m) (Identity w)

instance ErrorSemigroup o (Maybe m) (Maybe w)

main = log "Done"
