module TestEff where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Control.Monad.Eff.Error
import Debug.Trace

test1 = catchError (\s -> return 0) $ do
          trace "Testing"
          throwError "Error!"

test2 = runPure (runST (do
          ref <- newSTRef 0
          modifySTRef ref $ \n -> n + 1
          readSTRef ref))

module Main where

import Prelude
import Control.Monad.Eff
import TestEff

main = do
  n <- test1
  Debug.Trace.print n
  Debug.Trace.print test2
