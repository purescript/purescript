-- @shouldFailWith IncorrectAnonymousArgument
module Main where

import Prelude

test = ( _ * 4 + 1 ) 50
