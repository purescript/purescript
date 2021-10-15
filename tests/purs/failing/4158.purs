-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude

data Maybe a = Just a | Nothing

evil :: forall a b. Maybe (Record (foo :: Int | a)) -> Maybe (Record (foo :: Int | b))
evil r = r
