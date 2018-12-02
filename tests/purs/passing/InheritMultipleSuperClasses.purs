module Main where

import Prelude
import Effect.Console (log)

class (Functor f, Functor g) <= Eg1 f g

f1 :: forall f g. Eg1 f g => f ~> f
f1 = map identity -- Err, No type class instance was found for Functor f

g1 :: forall f g. Eg1 f g => g ~> g
g1 = map identity -- ok


class (Functor g, Functor f) <= Eg2 f g

f2 :: forall f g. Eg2 f g => f ~> f
f2 = map identity -- ok

g2 :: forall f g. Eg2 f g => g ~> g
g2 = map identity -- Err, No type class instance was found for Functor g


main = log "Done"

