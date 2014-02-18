module Tests where

import Prelude
import Eff
import Errors
import Trace
import Global

test1 n = runPure (catchError (\s -> return 0) $ do
  case {} of
    _ | n > 10 -> do
      throwError "Error!"
    _ -> return n)

test2 = do
  trace "Hello World"
  throwError "Error!"

test3 n = catchError (\s -> return 0) $ do
  case {} of
    _ | n > 10 -> do
      trace "n > 10"
      throwError "Error!"
    _ -> return n

test4 = do
  trace "Hello World!"
  return 0

test5 _ = do
  trace "Hello World!"
  test5 {}

test6 s = do
  trace s
  test6 (s ++ " test")

import ST

test7 _ = do
  n <- runST (do
    r <- newSTRef 0
    modifySTRef r $ \n -> n + 1
    readSTRef r)
  print n

test8 _ = (\_1 _2 -> {}) <$> print "Test" <*> print 8

test9 _ = runST (do
  r <- newSTRef 1
  whileE (return true) $ do
    n <- readSTRef r
    trace $ "Count " ++ show n
    modifySTRef r $ (+) 1)

module Main where

import Tests

main = test5 {}
