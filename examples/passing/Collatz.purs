module Main where

import Prelude
import Effect
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Effect.Console (log, logShow)

collatz :: Int -> Int
collatz n = ST.run (do
  r <- STRef.new n
  count <- STRef.new 0
  ST.while (map (_ /= 1) (STRef.read r)) do
    _ <- STRef.modify (_ + 1) count
    m <- STRef.read r
    void $ STRef.write (if m `mod` 2 == 0 then m / 2 else 3 * m + 1) r
  STRef.read count)

main = do
  logShow $ collatz 1000
  log "Done"
