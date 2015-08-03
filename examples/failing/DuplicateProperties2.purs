-- @shouldFailWith DuplicateLabel
module DuplicateProperties where

import Prelude

foreign import data Test :: # * -> *

foreign import subtractX :: forall r. Test (x :: Unit | r) -> Test r

foreign import hasX :: forall r. Test (x :: Unit, y :: Unit | r)

baz = subtractX (subtractX hasX)
