-- @shouldFailWith PartiallyAppliedSynonym
module Main where

import Prelude

class C (a :: (~>) Array)
