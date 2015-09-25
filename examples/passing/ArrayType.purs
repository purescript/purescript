module Main where

import Prelude

class Pointed p where
  point :: forall a. a -> p a

instance pointedArray :: Pointed Array where
  point a = [a]

main = Control.Monad.Eff.Console.log "Done"
