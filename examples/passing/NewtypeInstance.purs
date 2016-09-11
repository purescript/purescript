module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

newtype X = X String

newtype instance showX :: Show X

newtype instance eqX :: Eq X

newtype instance ordX :: Ord X

newtype Y a = Y (Array a)

newtype instance showY :: Show (Y String)

main = do
  logShow (X "test")
  logShow (Y ["test"])
  log "Done"
