module Tests where

import Prelude
import Eff
import Errors
import Trace
 
test1 n = runPure $ catchError (\s -> eff do return 0) $ eff do 
  case {} of 
    _ | n > 10 -> eff do
      throwError "Error!" 
    _ -> eff do return n

test2 = eff do
  trace "Hello World"
  throwError "Error!"

test3 n = catchError (\s -> eff do return 0) $ eff do 
  case {} of 
    _ | n > 10 -> eff do
      trace "n > 10"
      throwError "Error!" 
    _ -> eff do return n

test4 = eff do
  trace "Hello World!"
  return 0

test5 = eff do
  trace "Hello World!"
  test5
