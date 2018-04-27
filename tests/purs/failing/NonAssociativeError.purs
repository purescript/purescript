-- @shouldFailWith NonAssociativeError
-- @shouldFailWith NonAssociativeError
module Main where

import Prelude

a = true == true == true
b = true == false /= true
