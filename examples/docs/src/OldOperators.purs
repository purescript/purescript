
-- Remove this after 0.9.
module OldOperators (module OldOperators2) where

import OldOperators2

module OldOperators2 where

(>>) :: forall a. a -> a -> a
(>>) a b = b
