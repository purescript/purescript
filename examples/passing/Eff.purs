module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Control.Monad.Eff.Console

test1 = do
  log "Line 1"
  log "Line 2"

test2 = runPure (runST (do
          ref <- newSTRef 0.0
          modifySTRef ref $ \n -> n + 1.0
          readSTRef ref))

test3 = pureST (do
          ref <- newSTRef 0.0
          modifySTRef ref $ \n -> n + 1.0
          readSTRef ref)

main = do
  test1
  Control.Monad.Eff.Console.print test2
  Control.Monad.Eff.Console.print test3
