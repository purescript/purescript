module Main where

import Effect.Console

data Nil
data Snoc xs x

infixl 1 type Snoc as :>

type One = Nil :> Int
type Two = Nil :> Int :> Int
type Three = Nil :> Int :> Int :> Int

main = log "Done"
