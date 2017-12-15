-- @shouldFailWith NoInstanceFound

module Main where

import Prelude (negate)

-- Currently this is failing with "Unable to find instance Ring UInt", which is
-- correct, but it'd be nicer if it could fail with "Can't have negative UInt
-- literals"
n :: UInt
n = -1u
