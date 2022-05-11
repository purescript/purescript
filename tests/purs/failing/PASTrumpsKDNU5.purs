-- @shouldFailWith PartiallyAppliedSynonym
module Main where

import Prelude

newtype N = N ((~>) Array)
