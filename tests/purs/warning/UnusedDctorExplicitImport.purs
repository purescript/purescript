-- @shouldWarnWith UnusedDctorExplicitImport
module Main where

import Data.Ordering (Ordering(EQ, LT))

f :: Ordering -> Ordering
f EQ = EQ
f x = x
