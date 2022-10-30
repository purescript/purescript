module TestPsci where


import TestPsci.CommandTest (commandTests)
import TestPsci.CompletionTest (completionTests)
import TestPsci.EvalTest (evalTests)

import Test.Hspec

spec :: Spec
spec = do
  completionTests
  commandTests
  evalTests
