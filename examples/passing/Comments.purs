module Main where

-- | A comment on a single line
a = 1

-- | A comment
-- on multiple lines
b = 2

-- |
-- A comment with trailing lines
--
c = 3

-- | Another comment
data X =
  -- | a comment on a single line
  X |
  -- | A comment
  -- on multiple lines
  Y |
  -- | A comment
  -- with trailing lines
  --
  Z

main = Debug.Trace.trace "Done"

-- | A standalone comment
