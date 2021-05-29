-- @shouldFailWith OverlappingInstances
module Main where

class Test a where
  test :: a -> a

instance Test a where
  test x = x

instance Test Int where
  test _ = 0

-- The OverlappingInstances instances error only arises when there are two
-- choices for a dictionary, not when the instances are defined. So without
-- `value` this module would not raise an error.
value :: Int
value = test 1
