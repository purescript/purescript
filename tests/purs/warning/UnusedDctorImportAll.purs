-- @shouldWarnWith UnusedDctorImport
module Main where

import Data.Ordering (Ordering(..))

f :: Ordering -> Ordering
f x = x
