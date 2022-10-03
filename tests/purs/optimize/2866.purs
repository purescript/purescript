-- Canonical test for #2866. This doesn't need to test whether `apply`s
-- defined from modules other than `Data.Function` are incorrectly
-- optimized since the rest of the test suite seemingly catches it.
module Main where

import Prelude

newtype Area = Area Int

area = Area $ 42

areaFlipped = 42 # Area
