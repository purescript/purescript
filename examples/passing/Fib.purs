module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST

main = runST (do
  n1 <- newSTRef 1.0
  n2 <- newSTRef 1.0
  whileE ((>) 1000.0 <$> readSTRef n1) $ do
    n1' <- readSTRef n1
    n2' <- readSTRef n2
    writeSTRef n2 $ n1' + n2'
    writeSTRef n1 n2'
    Control.Monad.Eff.Console.print n2')
