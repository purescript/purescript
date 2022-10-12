-- @shouldFailWith PartiallyAppliedSynonym
module Main where

import Prelude

type T (a :: (~>) Array) = Int
