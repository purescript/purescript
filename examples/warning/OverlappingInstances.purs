-- @shouldWarnWith OverlappingInstances
module Main where

class Test a where
  test :: a -> a

instance testRefl :: Test a where
  test x = x

instance testInt :: Test Int where
  test _ = 0

-- The OverlappingInstances instances warning only arises when there are two
-- choices for a dictionary, not when the instances are defined. So without
-- `value` this module would not raise a warning.
value :: Int
value = test 1
