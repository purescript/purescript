-- @shouldFailWith TypesDoNotUnify
module DuplicateProperties where

import Prelude

foreign import data Test :: # Type -> Type

foreign import subtractX :: forall r. Test (x :: Unit | r) -> Test r

foreign import hasX :: Test (x :: Unit, y :: Unit)

baz = subtractX (subtractX hasX)
