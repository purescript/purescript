-- @shouldFailWith NoInstanceFound

module Issue1310 where

import Prelude
import Effect
import Effect.Console

class Inject f g where
  inj :: forall a. f a -> g a

instance inject :: Inject f f where
  inj x = x

newtype Oops a = Oops (Effect a)

main :: Effect Unit
main = inj (Oops (log "Oops"))
