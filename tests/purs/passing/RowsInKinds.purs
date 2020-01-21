module Main where

import Effect.Console (log)

foreign import data R :: forall k. Row k -> Type
foreign import data X :: forall r. R (x :: Type | r)
foreign import data Y :: forall r. R (y :: Type | r)

data P :: R (x :: Type, y :: Type) -> Type
data P a = P

type Test1 = P X
type Test2 = P Y

main = log "Done"
