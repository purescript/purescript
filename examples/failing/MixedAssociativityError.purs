-- @shouldFailWith MixedAssociativityError
module Main where

import Prelude

feq f x y = f <$> x == f <$> y
