-- @shouldFailWith TypesDoNotUnify
module Main where

import Safe.Coerce (coerce)

foreign import data Foreign :: Type -> Type -> Type -> Type

foreignToForeign :: forall a b c d. Foreign a b c -> Foreign a b d
foreignToForeign = coerce
