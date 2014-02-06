module TestEff where

import Prelude
import Eff
import ST
import Errors
import Trace

test1 = catchError (\s -> ret 0) $ do
          trace "Testing"
          throwError "Error!"

test2 = runPure (runST (do
          ref <- newSTRef 0
          modifySTRef ref $ \n -> n + 1
          readSTRef ref))

module Main where

import Prelude
import Eff
import TestEff

main = do
  n <- test1
  Trace.print n
  Trace.print test2
