-- @shouldFailWith PartiallyAppliedSynonym
module Main where

import Prelude

data D (a :: (~>) Array) = D
