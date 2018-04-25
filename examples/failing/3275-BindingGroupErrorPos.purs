-- @shouldFailWith KindsDoNotUnify
module BindingGroupErrorPos where

-- This isn't really about KindsDoNotUnify, it's about positioning errors
-- that occur in binding groups

import Prelude

type Result = Array Int

wrong :: Int -> Result String
wrong n = wrong (n - 1)
