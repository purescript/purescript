module Lib where

import Prelude

import Effect (Effect)

class NamedExportStillWorks a where
  doTest :: Effect a

-- This test expects the generated name of this instance to be
-- namedExportStillWorksUnit in the absence of another identifier with that
-- name (as we have here).
-- The test ensures that the instance doesn't preempt the named declaration.
-- (If the naming scheme for unnamed instances ever changes, the name of the
-- exported declaration in this test should change with it.)
instance NamedExportStillWorks Unit where
  doTest = pure unit

namedExportStillWorksUnit :: Int -> Effect Unit
namedExportStillWorksUnit _ = doTest
