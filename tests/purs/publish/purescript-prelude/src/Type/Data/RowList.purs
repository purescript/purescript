module Type.Data.RowList where

import Prim.RowList (kind RowList)

-- | A proxy data type whose type parameter is a type of kind `RowList`.
-- |
-- | Commonly used for specialising a function with a quantified type.
data RLProxy (rowlist :: RowList)
  = RLProxy
