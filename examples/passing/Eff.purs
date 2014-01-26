module TestEff where

import Prelude
import Eff
import ST
import Errors
import Trace

test1 = catchError (\s -> ret 0) 
  (do trace "Testing"
      throwError "Error!")

test2 = runPure (runST (do
  ref <- newSTRef 0
  modifySTRef (\n -> n + 1) ref 
  readSTRef ref))

module Main where

import Eff
import TestEff
import Global

main = do
  n <- test1
  Trace.print n
  Trace.print test2
