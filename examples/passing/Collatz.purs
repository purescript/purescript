module Main where

import Prelude
import Eff
import ST

collatz :: Number -> Number
collatz n = runPure (runST (do
  r <- newSTRef n
  count <- newSTRef 0
  untilE $ do
    modifySTRef count $ (+) 1
    m <- readSTRef r
    writeSTRef r $ if m % 2 == 0 then m / 2 else 3 * m + 1
    return $ m == 1
  readSTRef count))

main = Trace.print $ collatz 1
