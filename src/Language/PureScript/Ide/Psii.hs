-- |
-- `psii` stands for `PureScript IDE Information`.
--
-- Q: How to read `psii`?
-- A: As in "psy" or "sai".
module Language.PureScript.Ide.Psii where

import Protolude

-- |
-- The verbosity of IDE information.
data PsiiVerbosity
  -- | Include no information at all.
  = PsiiNoInformation
  -- | Include top-level information.
  | PsiiOnlyTopLevel
  -- | Include local information.
  | PsiiWithLocal
  deriving (Show, Eq, Ord)

-- |
-- The information entry to be emitted.
data PsiiInformation
  -- | A declaration in a source file.
  = PsiiDeclaration
  -- | A reference to a declaration.
  | PsiiReference
  deriving (Show, Eq, Ord)
