-- @shouldFailWith TypesDoNotUnify
module Main where

import Safe.Coerce (coerce)

foreign import data Foreign :: ∀ k. k -> k -> Type

foreignToForeign :: ∀ k (a :: k) (b :: k) (c :: k). Foreign a b -> Foreign a c
foreignToForeign = coerce
