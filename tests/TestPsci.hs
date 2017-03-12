{-# LANGUAGE RecordWildCards #-}

module TestPsci where

import Prelude ()
import Prelude.Compat

import Control.Monad (when)
import System.Exit (exitFailure)
import Test.HUnit
import TestPsci.CommandTest (commandTests)
import TestPsci.CompletionTest (completionTests)

main :: IO ()
main = do
  Counts{..} <- runTestTT allTests
  when (errors + failures > 0) exitFailure

allTests :: Test
allTests = TestList [ completionTests
                    , commandTests
                    ]
