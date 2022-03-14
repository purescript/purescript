module Main where

-- | Not a number (NaN)
foreign import nan :: Number

-- | Test whether a number is NaN
foreign import isNaN :: Number -> Boolean

-- | Positive infinity
foreign import infinity :: Number

-- | Test whether a number is finite
foreign import isFinite :: Number -> Boolean

