module Language.PureScript.Docs.Utils.MonoidExtras where

import Data.Monoid

mintersperse :: (Monoid m) => m -> [m] -> m
mintersperse _ []       = mempty
mintersperse _ [x]      = x
mintersperse sep (x:xs) = x <> sep <> mintersperse sep xs

