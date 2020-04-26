-- | Module Documentation
module CompletionSpecDocs where

-- | Doc x
something = "something"

-- | Doc *123*
withType :: Int
withType = 42

-- | This is
-- | a multi-line
-- | comment
multiline = "multiline"

-- | Doc for class
class DocClass where
  -- | doc for member
  member :: Int
