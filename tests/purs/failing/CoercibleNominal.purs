-- @shouldFailWith TypesDoNotUnify
module Main where

import Safe.Coerce (coerce)

data Nominal a (b :: Type) = Nominal a

type role Nominal nominal phantom

nominalToNominal :: forall a b c. Nominal a c -> Nominal b c
nominalToNominal = coerce
