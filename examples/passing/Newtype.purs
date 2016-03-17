module Main where

import Prelude hiding (apply)
import Control.Monad.Eff
import Control.Monad.Eff.Console

newtype Thing = Thing String

instance showThing :: Show Thing where
  show (Thing x) = "Thing " <> show x

newtype Box a = Box a

instance showBox :: (Show a) => Show (Box a) where
  show (Box x) = "Box " <> show x

apply f x = f x

main = do
  logShow $ Thing "hello"
  logShow $ Box 42.0
  logShow $ apply Box 9000.0
  log "Done"
