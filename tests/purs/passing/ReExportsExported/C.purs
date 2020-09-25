module C (module B) where

-- | `A.a` was re-exported from `B` and then again from `C`
import B
