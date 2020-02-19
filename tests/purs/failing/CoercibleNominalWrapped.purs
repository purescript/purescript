-- @shouldFailWith NoInstanceFound
module Main where

import Safe.Coerce (coerce)

data Nominal a b = Nominal a b

type role Nominal nominal phantom

newtype Id a = Id a

data Wrap a b = Wrap (Nominal a b)

wrapToWrap :: forall a b. Wrap a b -> Wrap (Id a) b
wrapToWrap = coerce
