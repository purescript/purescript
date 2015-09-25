-- @shouldFailWith NoInstanceFound

module Issue1310 where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

class Inject f g where
  inj :: forall a. f a -> g a

instance inject :: Inject f f where
  inj x = x

foreign import data Oops :: !

main :: forall eff. Eff (oops :: Oops | eff) Unit
main = inj (log "Oops")
