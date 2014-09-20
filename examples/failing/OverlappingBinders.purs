module OverlappingBinders where

f x = case x of
  (y:y@(z:zs)) -> y
