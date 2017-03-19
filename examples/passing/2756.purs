module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)
import Prelude

pu :: forall eff. Eff eff Unit
pu = pure unit

type C eff = { pu :: Eff eff Unit }

sampleC :: C ()
sampleC = { pu: pu }

newtype Identity a = Id a

sampleIdC :: Identity (C ())
sampleIdC = Id { pu : pu }

main = log "Done"
