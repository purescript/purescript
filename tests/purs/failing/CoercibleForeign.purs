-- @shouldFailWith TypesDoNotUnify
module Main where

import Safe.Coerce (coerce)

foreign import data Foreign :: Type -> Type -> Type

newtype Id a = Id a

foreignToForeign :: forall a b. Foreign a b -> Foreign (Id a) (Id b)
foreignToForeign = coerce
