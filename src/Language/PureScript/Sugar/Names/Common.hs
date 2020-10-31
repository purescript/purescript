module Language.PureScript.Sugar.Names.Common (warnDuplicateRefs) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Monad.Writer (MonadWriter(..))

import Data.Foldable (for_)
import Data.List (group, sort, (\\))
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
      dupeRefs = mapMaybe (refToName pos) $ removeUnique withoutCtors
      dupeCtors = concat $ mapMaybe (extractCtors pos) refs

  for_ (dupeRefs ++ dupeCtors) $ \(pos', name) ->
    warnWithPosition pos' . tell . errorMessage $ toError name

  where

  -- Removes all unique elements from list
  -- as well as one of each duplicate.
  -- Example:
  --  removeUnique [1,2,2,3,3,3,4] == [2,3,3]
  -- Note that it may be more correct to keep ALL duplicates,
  -- but that requires additional changes in how warnings are printed.
  -- Example of keeping all duplicates (not what this code currently does):
  --  removeUnique [1,2,2,3,3,3,4] == [2,2,3,3,3]
  removeUnique :: Eq a => Ord a => [a] -> [a]
  removeUnique = concatMap (drop 1) . group . sort

  -- Deletes the constructor information from TypeRefs so that only the
  -- referenced type is used in the duplicate check - constructors are handled
  -- separately
  deleteCtors :: DeclarationRef -> DeclarationRef
  deleteCtors (TypeRef sa pn _) = TypeRef sa pn Nothing
  deleteCtors other = other

  -- Extracts the names of duplicate constructor references from TypeRefs.
  extractCtors :: SourceSpan -> DeclarationRef -> Maybe [(SourceSpan, Name)]
  extractCtors pos' (TypeRef _ _ (Just dctors)) =
    let dupes = dctors \\ ordNub dctors
    in if null dupes then Nothing else Just $ ((pos',) . DctorName) <$> dupes
  extractCtors _ _ = Nothing

  -- Converts a DeclarationRef into a name for an error message.
  refToName :: SourceSpan -> DeclarationRef -> Maybe (SourceSpan, Name)
  refToName pos' (TypeRef _ name _) = Just (pos', TyName name)
  refToName pos' (TypeOpRef _ op) = Just (pos', TyOpName op)
  refToName pos' (ValueRef _ name) = Just (pos', IdentName name)
  refToName pos' (ValueOpRef _ op) = Just (pos', ValOpName op)
  refToName pos' (TypeClassRef _ name) = Just (pos', TyClassName name)
  refToName pos' (ModuleRef _ name) = Just (pos', ModName name)
  refToName _ _ = Nothing
