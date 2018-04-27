module Main where

import Prelude
import Effect.Console (log)

class Pointed p where
  point :: forall a. a -> p a

instance pointedArray :: Pointed Array where
  point a = [a]

main = log "Done"
