-- |
-- `psii` stands for `PureScript IDE Information`.
--
-- Q: How to read `psii`?
-- A: As in "psy" or "sai".
module Language.PureScript.Ide.Psii where

import Protolude

import Data.String (String)
import qualified Data.Text as T

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Pretty
import Language.PureScript.Types

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
  -- |
  -- Value declarations.
  = PsiiValueDecl PsiiValueDecl'
  deriving (Show, Eq, Ord)

data PsiiValueDecl' = PsiiValueDecl'
  { psiiValueDeclSpan :: SourceSpan
  , psiiValueDeclIdent :: Qualified Ident
  , psiiValueDeclType :: SourceType
  }
  deriving (Show, Eq, Ord)

debugInformation :: PsiiInformation -> String
debugInformation (PsiiValueDecl (PsiiValueDecl' {..})) =
  let
    ident' = runIdent $ disqualify psiiValueDeclIdent
    type' = T.strip $ T.pack $ prettyPrintType maxBound psiiValueDeclType
  in
    T.unpack $ "(" <> ident' <> " :: " <> type' <> ")"
