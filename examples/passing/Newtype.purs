module Main where

import Control.Monad.Eff
import Debug.Trace

newtype Thing = Thing String

instance showThing :: Show Thing where
  show (Thing x) = "Thing " ++ show x

newtype Box a = Box a

instance showBox :: (Show a) => Show (Box a) where
  show (Box x) = "Box " ++ show x

main = do
  print $ Thing "hello"
  print $ Box 42
  trace "Done"