module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Debug.Trace

test1 = do
  trace "Line 1"
  trace "Line 2"

test2 = runPure (runST (do
          ref <- newSTRef 0
          modifySTRef ref $ \n -> n + 1
          readSTRef ref))

test3 = pureST (do
          ref <- newSTRef 0
          modifySTRef ref $ \n -> n + 1
          readSTRef ref)

main = do
  test1
  Debug.Trace.print test2
  Debug.Trace.print test3
