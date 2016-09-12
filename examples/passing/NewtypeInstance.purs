module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

newtype X = X String

derive newtype instance showX :: Show X

derive newtype instance eqX :: Eq X

derive newtype instance ordX :: Ord X

newtype Y a = Y (Array a)

derive newtype instance showY :: Show (Y String)

main = do
  logShow (X "test")
  logShow (Y ["test"])
  log "Done"
