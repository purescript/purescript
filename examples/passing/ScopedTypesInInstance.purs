module Main where

import Prelude
import Control.Monad.Eff.Console (log)

class Test t where
  test :: forall a. t -> a -> a

instance testUnit :: Test Unit where
  test :: forall a. Unit -> a -> a
  test = go where
    go :: Unit -> a -> a
    go _ a = a

main = log "Done"
