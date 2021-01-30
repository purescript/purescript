-- |
-- This module contains internal extensions to Data.Text.
--
module Data.Text.PureScript (spanUpTo) where

import Prelude

import Data.Text.Internal (Text(..), text)
import Data.Text.Unsafe (Iter(..), iter)

-- | /O(n)/ 'spanUpTo', applied to a number @n@, predicate @p@, and text @t@,
-- returns a pair whose first element is the longest prefix (possibly empty) of
-- @t@ of length less than or equal to @n@ of elements that satisfy @p@, and
-- whose second is the remainder of the text.
{-# INLINE spanUpTo #-}
spanUpTo :: Int -> (Char -> Bool) -> Text -> (Text, Text)
spanUpTo n p t@(Text arr off len) = (hd, tl)
  where hd = text arr off k
        tl = text arr (off + k) (len - k)
        !k = loop n 0
        loop !n' !i | n' > 0 && i < len && p c = loop (n' - 1) (i + d)
                    | otherwise                = i
            where Iter c d = iter t i
