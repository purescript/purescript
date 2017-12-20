-- @shouldFailWith NegativeUInt

module Main where

import Prelude
import Data.Ring

-- This is failing with a NoInstanceFound error instead of NegativeUInt. I have
-- no idea why. I can load `pulp psci` in the directory and it works with UInts
-- just fine.
n :: UInt
n = -1u
