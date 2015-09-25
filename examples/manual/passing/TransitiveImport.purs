module Test where

  import Prelude

  class TestCls a where
    test :: a -> a

  instance unitTestCls :: TestCls Unit where
    test _ = unit

module Middle where

  middle = Test.test

module Main where

  import Prelude
  import Middle
  import Control.Monad.Eff.Console

  main = do
    print (middle unit)
    trace "Done"
    return unit
