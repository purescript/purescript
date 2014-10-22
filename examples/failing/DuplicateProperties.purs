module DuplicateProperties where

foreign import data Test :: # * -> *

foreign import subtractX "" :: forall r. Test (x :: Unit | r) -> Test r

foreign import hasX "" :: Test (x :: Unit, y :: Unit)

-- This type checks erroneously, and according to PSCI it has type Test (y :: Prelude.Unit)
baz :: Test (y :: Unit)
baz = subtractX (subtractX hasX)
