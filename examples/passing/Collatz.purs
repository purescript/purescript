module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Control.Monad.Eff.Console (log, logShow)

collatz :: Int -> Int
collatz n = runPure (runST (do
  r <- newSTRef n
  count <- newSTRef 0
  untilE $ do
    modifySTRef count $ (+) 1
    m <- readSTRef r
    writeSTRef r $ if m `mod` 2 == 0 then m / 2 else 3 * m + 1
    pure $ m == 1
  readSTRef count))

main = do
  logShow $ collatz 1000
  log "Done"
