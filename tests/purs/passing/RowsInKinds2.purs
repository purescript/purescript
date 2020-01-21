module Main where

import Effect.Console (log)

foreign import data R :: forall k. Row k -> Type
foreign import data X :: R ()

data P :: R () -> Type
data P a = P

type Test1 = P X

main = log "Done"
