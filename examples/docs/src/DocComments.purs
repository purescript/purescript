module DocComments where

-- | This declaration has a code block:
-- |
-- |     example == 0
-- |
-- | Here we are really testing that the leading whitespace is not stripped, as
-- | this ensures that we don't accidentally change code blocks into normal
-- | paragraphs.
example :: Int
example = 0
