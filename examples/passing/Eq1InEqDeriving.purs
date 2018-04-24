module Main where

import Prelude
import Data.Eq (class Eq1)
import Effect.Console (log)

newtype Mu f = In (f (Mu f))

derive instance eqMu :: Eq1 f => Eq (Mu f)

main = log "Done"
