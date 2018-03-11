-- @shouldFailWith KindsDoNotUnify
module KindsDoNotUnifySpan where

import Prelude

type Result = Array Int

wrong :: Int -> Result String
wrong n = wrong (n - 1)
