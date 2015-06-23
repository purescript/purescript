module OverlappingInstances where

import Prelude

data A = A

instance showA1 :: Show A where
  show A = "Instance 1"

instance showA2 :: Show A where
  show A = "Instance 2"

main = Debug.Trace.trace $ show A
