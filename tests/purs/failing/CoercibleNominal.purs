-- @shouldFailWith NoInstanceFound
module Main where

import Safe.Coerce (coerce)

data Nominal a b = Nominal a b

type role Nominal nominal phantom

nominalToNominal :: forall a b c. Nominal a c -> Nominal b c
nominalToNominal = coerce
