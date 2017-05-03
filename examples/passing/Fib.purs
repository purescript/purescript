module Main where

import Prelude
import Control.Monad.Eff (whileE)
import Control.Monad.Eff.Console (log, logShow)
import Control.Monad.ST (runST, newSTRef, readSTRef, writeSTRef)

main = do
  runST do
    n1 <- newSTRef 1.0
    n2 <- newSTRef 1.0
    whileE ((>) 1000.0 <$> readSTRef n1) $ do
      n1' <- readSTRef n1
      n2' <- readSTRef n2
      _ <- writeSTRef n2 $ n1' + n2'
      _ <- writeSTRef n1 n2'
      logShow n2'
  log "Done"
