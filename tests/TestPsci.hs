module TestPsci where

import Prelude ()
import Prelude.Compat

import TestPsci.CommandTest (commandTests)
import TestPsci.CompletionTest (completionTests)
import TestPsci.EvalTest (evalTests)

import Test.Tasty
import Test.Tasty.Hspec

main :: IO TestTree
main = testSpec "repl" $ do
  completionTests
  commandTests
  evalTests
