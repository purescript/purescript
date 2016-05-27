module Main where

import Prelude
import Control.Monad.Eff.Console (log)

class Show a <= Nonsense a where
  method :: a -> a

data Box a = Box a

instance showBox :: Show a => Show (Box a) where
  show (Box a) = "Box " <> show a

strangeThing :: forall m. Semigroup (m Unit) => m Unit -> m Unit -> m Unit
strangeThing x y = x <> y

main = log "Done"
