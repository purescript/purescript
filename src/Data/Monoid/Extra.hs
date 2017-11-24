-- FIXME move to Data.Monoid.Extra?
module Data.Monoid.Extra where

import PSPrelude

mintersperse :: (Monoid m) => m -> [m] -> m
mintersperse _ []       = mempty
mintersperse _ [x]      = x
mintersperse sep (x:xs) = x <> sep <> mintersperse sep xs

-- |
-- Generalize intercalate slightly for monoids
--
mintercalate :: Monoid m => m -> [m] -> m
mintercalate x xs = mconcat (intersperse x xs)
