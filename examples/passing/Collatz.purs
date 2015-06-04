module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST

collatz :: Number -> Number
collatz n = runPure (runST (do
  r <- newSTRef n
  count <- newSTRef 0.0
  untilE $ do
    modifySTRef count $ (+) 1.0
    m <- readSTRef r
    writeSTRef r $ if m % 2.0 == 0.0 then m / 2.0 else 3.0 * m + 1.0
    return $ m == 1.0
  readSTRef count))

main = Debug.Trace.print $ collatz 1000.0
