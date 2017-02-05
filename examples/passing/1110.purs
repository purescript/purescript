module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data X a = X

x :: forall a. X a
x = X
	 
type Y = { x :: X Int }

test :: forall m. Monad m => m Y
test = pure { x: x }

main = log "Done"
