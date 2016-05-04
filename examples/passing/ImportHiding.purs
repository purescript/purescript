module Main where

import Control.Monad.Eff.Console
import Prelude hiding (
  show, -- a value
  class Show, -- a type class
  Unit(..)  -- a constructor
  )

show = 1.0

class Show a where
  noshow :: a -> a

data Unit = X | Y

main = do
  logShow show
  log "Done"
