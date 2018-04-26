-- @shouldFailWith NoInstanceFound
module Main where

import Safe.Coerce (coerce)

data Phantom a = Phantom

role Phantom representational

phantomToPhantom :: forall a b. Phantom a -> Phantom b
phantomToPhantom = coerce
