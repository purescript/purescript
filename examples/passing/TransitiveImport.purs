module Test where

  class TestCls a where
    test :: a -> a

  instance unitTestCls :: TestCls Unit where
    test _ = unit

module Middle where

  middle = Test.test

module Main where

  import Middle
  import Debug.Trace

  main = do
    print (middle unit)
    trace "Done"
    return unit
