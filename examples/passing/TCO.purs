module Main where

import Prelude
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.Rec.Class
import Data.Array ((..), span, length)

main = do
  let f x = x + 1
  let v = 0
  logShow (applyN 0 f v)
  logShow (applyN 1 f v)
  logShow (applyN 2 f v)
  logShow (applyN 3 f v)
  logShow (applyN 4 f v)

  let largeArray = 1..10000
  logShow (length (span (\_ -> true) largeArray).init)

  logShow (tailRec (\n -> if n < 10000 then Loop (n + 1) else Done 42) 0)

  log "Done"

applyN :: forall a. Int -> (a -> a) -> a -> a
applyN = go id
  where
  go f n _ | n <= 0 = f
  go f n g = go (f >>> g) (n - 1) g
