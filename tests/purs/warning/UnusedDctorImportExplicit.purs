-- @shouldWarnWith UnusedDctorImport
module Main where

import Data.Ordering (Ordering(EQ))

f :: Ordering -> Ordering
f x = x
