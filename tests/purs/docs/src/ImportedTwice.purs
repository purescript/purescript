-- See also an example in the wild: purescript-transformers v0.8.4.
-- Control.Monad.RWS.Trans re-exports `lift` from both Control.Monad.Trans
-- (where it is originally defined) and Control.Monad.RWS.Class (which
-- re-exports it from Control.Monad.Trans).

module ImportedTwice
  ( module ImportedTwiceA
  , module ImportedTwiceB
  )
  where

import ImportedTwiceA
import ImportedTwiceB
