-- @shouldFailWith TypesDoNotUnify
module Main where

import Safe.Coerce (coerce)

data Phantom a = Phantom

data Maybe a = Nothing | Just a

data G a b = G (a (Phantom b))

gToG :: G Maybe Int -> G Maybe String
gToG = coerce
