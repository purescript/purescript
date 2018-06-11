-- @shouldFailWith InvalidNewtype
module Main where

import Prelude

newtype Thing = Thing String | Other
