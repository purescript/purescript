module CustomAssert (assertThrows) where

import Prelude

import Effect (Effect)

assertThrows :: forall a. (Unit -> a) -> Effect String
assertThrows = assertThrowsImpl unit

foreign import assertThrowsImpl :: forall a b. a -> (a -> b) -> Effect String
