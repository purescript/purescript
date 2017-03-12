module Main where

import Lib (class X, go)
import Control.Monad.Eff.Console (log)

go' :: forall a. X a => a -> a
go' = go

main = log "Done"
