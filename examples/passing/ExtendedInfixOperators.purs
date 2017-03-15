module Main where

import Prelude
import Control.Monad.Eff.Console (log, logShow)
import Data.Function (on)

comparing :: forall a b. Ord b => (a -> b) -> a -> a -> Ordering
comparing f = compare `on` f

null [] = true
null _ = false

test = [1.0, 2.0, 3.0] `comparing null` [4.0, 5.0, 6.0]

main = do
  logShow test
  log "Done"
