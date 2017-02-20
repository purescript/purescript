module Language.PureScript.Sugar.Names.Common (warnDuplicateRefs) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Monad.Writer (MonadWriter(..))

import Data.Foldable (for_)
import Data.Function (on)
import Data.List (nubBy, (\\))
import Data.Maybe (mapMaybe)

import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Names

-- |
-- Warns about duplicate values in a list of declaration refs.
--
warnDuplicateRefs
  :: MonadWriter MultipleErrors m
  => SourceSpan
  -> (Name -> SimpleErrorMessage)
  -> [DeclarationRef]
  -> m ()
warnDuplicateRefs pos toError refs = do
  let withoutCtors = deleteCtors `map` refs
      dupeRefs = mapMaybe (refToName pos) $ withoutCtors \\ nubBy ((==) `on` withoutPosInfo) withoutCtors
      dupeCtors = concat $ mapMaybe (extractCtors pos) refs

  for_ (dupeRefs ++ dupeCtors) $ \(pos', name) ->
    warnWithPosition pos' . tell . errorMessage $ toError name

  where

  -- Returns a DeclarationRef unwrapped from any PositionedDeclarationRef
  -- constructor(s) it may be wrapped within. Used so position info is ignored
  -- when making the comparison for duplicates.
  withoutPosInfo :: DeclarationRef -> DeclarationRef
  withoutPosInfo (PositionedDeclarationRef _ _ ref) = withoutPosInfo ref
  withoutPosInfo other = other

  -- Deletes the constructor information from TypeRefs so that only the
  -- referenced type is used in the duplicate check - constructors are handled
  -- separately
  deleteCtors :: DeclarationRef -> DeclarationRef
  deleteCtors (PositionedDeclarationRef ss com ref) =
    PositionedDeclarationRef ss com (deleteCtors ref)
  deleteCtors (TypeRef pn _) = TypeRef pn Nothing
  deleteCtors other = other

  -- Extracts the names of duplicate constructor references from TypeRefs.
  extractCtors :: SourceSpan -> DeclarationRef -> Maybe [(SourceSpan, Name)]
  extractCtors _ (PositionedDeclarationRef pos' _ ref) = extractCtors pos' ref
  extractCtors pos' (TypeRef _ (Just dctors)) =
    let dupes = dctors \\ ordNub dctors
    in if null dupes then Nothing else Just $ ((pos',) . DctorName) <$> dupes
  extractCtors _ _ = Nothing

  -- Converts a DeclarationRef into a name for an error message.
  refToName :: SourceSpan -> DeclarationRef -> Maybe (SourceSpan, Name)
  refToName pos' (TypeRef name _) = Just (pos', TyName name)
  refToName pos' (TypeOpRef op) = Just (pos', TyOpName op)
  refToName pos' (ValueRef name) = Just (pos', IdentName name)
  refToName pos' (ValueOpRef op) = Just (pos', ValOpName op)
  refToName pos' (TypeClassRef name) = Just (pos', TyClassName name)
  refToName pos' (ModuleRef name) = Just (pos', ModName name)
  refToName _ (PositionedDeclarationRef pos' _ ref) = refToName pos' ref
  refToName _ _ = Nothing
