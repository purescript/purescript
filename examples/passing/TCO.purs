module Main where

import Prelude
import Control.Monad.Eff.Console (log, logShow)

main = do
  let f x = x + 1
  let v = 0
  logShow (applyN 0 f v)
  logShow (applyN 1 f v)
  logShow (applyN 2 f v)
  logShow (applyN 3 f v)
  logShow (applyN 4 f v)
  log "Done"

applyN :: forall a. Int -> (a -> a) -> a -> a
applyN = go id
  where
  go f n _ | n <= 0 = f
  go f n g = go (f >>> g) (n - 1) g
