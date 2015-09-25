-- @shouldFailWith OverlappingArgNames
module OverlappingBinders where

import Prelude

data S a = S a (S a)

f x = case x of
  (S y (S y@(S z zs))) -> y
