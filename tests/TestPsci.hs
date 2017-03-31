module TestPsci where

import Prelude ()
import Prelude.Compat

import Test.Hspec
import TestPsci.CommandTest (commandTests)
import TestPsci.CompletionTest (completionTests)
import TestPsci.EvalTest (evalTests)

main :: IO ()
main = hspec $ do
  completionTests
  commandTests
  evalTests
