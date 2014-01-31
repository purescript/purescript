module Tests where

import Prelude
import Eff
import Errors
import Trace
import Global
 
test1 n = runPure $ catchError (\s -> ret 0) $ do 
  case {} of 
    _ | n > 10 -> do
      throwError "Error!" 
    _ -> ret n

test2 = do
  trace "Hello World"
  throwError "Error!"

test3 n = catchError (\s -> ret 0) $ do 
  case {} of 
    _ | n > 10 -> do
      trace "n > 10"
      throwError "Error!" 
    _ -> ret n

test4 = do
  trace "Hello World!"
  ret 0

test5 = do
  trace "Hello World!"
  test5

test6 s = do
  trace s
  test6 (s ++ " test")

import ST

test7 = \ -> do
  n <- runST $ do
    r <- newSTRef 0
    modifySTRef r $ \n -> n + 1
    readSTRef r
  print $ n

module Main where

import Tests

main = test7 ()
