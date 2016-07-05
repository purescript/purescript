module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

head :: forall a. Partial => Array a -> a
head [x] = x

main :: Eff _ _
main = log "Done"
