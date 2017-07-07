module Main where

import Control.Monad.Eff.Console (log)
import Prelude (class Show, show)

type I t = t

newtype Id t = Id t

instance foo :: Show (I t) => Show (Id t) where
  show (Id t) = "Done"

main = log (show (Id "other"))
