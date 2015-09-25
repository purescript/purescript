-- @shouldFailWith PropertyIsMissing
module Main where

import Prelude

test o = o.foo

test1 = test {}
