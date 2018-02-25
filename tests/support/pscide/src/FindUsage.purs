module FindUsage where

import FindUsage.Definition (usageId, ($%), Usage(..))
import FindUsage.Reexport (toBeReexported)

usagePatternMatch ∷ Usage → Usage
usagePatternMatch x = case x of
  Used _ → x
  _ $% _ → x

usageFn ∷ ∀ a. a → a
usageFn = usageId toBeReexported
