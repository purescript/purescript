module Data.Ord.Unsafe (unsafeCompare) where

import Data.Ordering (Ordering(..))

unsafeCompare :: forall a. a -> a -> Ordering
unsafeCompare = unsafeCompareImpl LT EQ GT

foreign import unsafeCompareImpl
  :: forall a
   . Ordering
  -> Ordering
  -> Ordering
  -> a
  -> a
  -> Ordering
