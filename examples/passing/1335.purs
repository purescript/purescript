module Main where

import Prelude
import Control.Monad.Eff.Console (log)

x :: forall a. a -> String
x a = y "Test"
  where
  y :: forall a. (Show a) => a -> String
  y a = show (a :: a)

main = do
  log (x 0)
  log "Done"
