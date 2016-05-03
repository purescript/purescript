module Main where

import Control.Monad.Eff.Console (log)
import Lib (class X, go)

go' :: forall a. (X a) => a -> a
go' = go

main = log "Done"
