module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST

collatz :: Int -> Int
collatz n = runPure (runST (do
  r <- newSTRef n
  count <- newSTRef 0
  untilE $ do
    modifySTRef count $ (+) 1
    m <- readSTRef r
    writeSTRef r $ if m `mod` 2 == 0 then m / 2 else 3 * m + 1
    return $ m == 1
  readSTRef count))

main = Control.Monad.Eff.Console.print $ collatz 1000
