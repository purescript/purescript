module SkolemEscape where

import Prelude
import Eff
import ST

test _ = do
  r <- runST (newSTRef 0)
  ret 0 
