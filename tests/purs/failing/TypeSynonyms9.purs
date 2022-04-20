-- @shouldFailWith PartiallyAppliedSynonym
module Main where

import Prelude

newtype A (a :: (Type -> Type) -> Type -> Type) = A String
newtype B = B (A ((~>) Array))
