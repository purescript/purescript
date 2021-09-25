module Main where

import Effect.Console (log)
import Other (Id)

type Alias = Int

type Wrapped :: forall k. (Type -> k) -> Row k -> Row k
type Wrapped f r = (key :: f Alias | r)

type Unwrapped :: Row Type -> Row Type
type Unwrapped r = Wrapped Id r

main = log "Done"
