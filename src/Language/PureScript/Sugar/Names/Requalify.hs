-- | This module provides functions for "re-qualifying" names in a type given
-- an Imports record, so that they can be displayed unambiguously in
-- diagnostics and suggestions without unnecessary qualification. That is,
-- given a fully qualified name like `Foo.Bar.X`, this module lets you go to
-- simply `X` (no qualification) if the given Imports record includes an open
-- import for `X` from `Foo.Bar`, or alternatively if `Foo.Bar` is imported
-- like `import Foo.Bar as F` then it produces `F.X`.
module Language.PureScript.Sugar.Names.Requalify
  ( ReverseImports(..)
  , buildReverseImports
  , requalify
  , requalifyConstraint
  , reverseLookup
  , requalifyTypesInErrors
  ) where

import Debug.Trace
import Prelude
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Language.PureScript.Constants.Prim as Prim
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Sugar.Names.Env

type ReverseImportMap a = M.Map (ModuleName, a) QualifiedBy

-- | Like an Imports record, except this only contains type and type operator
-- names, and is reversed - while an Imports record maps local names to fully
-- qualified names, this data type maps fully qualified names to local names.
-- This enables 'requalifying' qualified names with their locally qualified
-- names if any, or no qualification if they were imported open
data ReverseImports = ReverseImports
  { reverseImportsTypes :: ReverseImportMap (ProperName 'TypeName)
  , reverseImportsTypeOps :: ReverseImportMap (OpName 'TypeOpName)
  , reverseImportsTypeClasses :: ReverseImportMap (ProperName 'ClassName)
  }
  deriving (Show)

buildReverseImports :: Imports -> ReverseImports
buildReverseImports (Imports { importedTypes, importedTypeOps, importedTypeClasses }) =
  trace (show revimps) revimps
  where
  revimps = ReverseImports
    { reverseImportsTypes =
        M.fromListWith preferShortest $
          mapMaybe (toReverseAssoc typeExceptions) $
            M.toList importedTypes
    , reverseImportsTypeOps =
        M.fromListWith preferShortest $
          mapMaybe (toReverseAssoc typeOpExceptions) $
            M.toList importedTypeOps
    , reverseImportsTypeClasses =
        M.fromListWith preferShortest $
          mapMaybe (toReverseAssoc typeClassExceptions) $
            M.toList importedTypeClasses
    }

  preferShortest :: QualifiedBy -> QualifiedBy -> QualifiedBy
  preferShortest x@(BySourcePos _) _ = x
  preferShortest _ y@(BySourcePos _) = y
  preferShortest (ByModuleName x) (ByModuleName y) = do
    ByModuleName $
      if T.length (runModuleName x) < T.length (runModuleName y)
        then x
        else y

  -- | Given an entry from an ImportMap from an Imports (see Sugar.Names.Env)
  -- which maps a local name to a fully qualified name, produce an entry for
  -- the corresponding ReverseImports.
  --
  -- TODO does this need to care about situations where this is ambiguous i.e.
  -- there are two different declarations that a given name could refer to? I
  -- think the answer is yes. Eg:
  --
  -- import Foo as X
  -- import Bar as X
  --
  -- is fine, as long as you don't do X.whatever for anything that both modules
  -- export
  --
  -- We make exceptions for anything that has special cases in the type
  -- pretty-printer such as Record and Function
  toReverseAssoc :: Ord a => Set (Qualified a) -> (Qualified a, [ImportRecord a]) -> Maybe ((ModuleName, a), QualifiedBy)
  toReverseAssoc exceptions (Qualified localModName name, importRecords) =
    case importRecords of
      [] ->
        -- Probably shouldn't happen
        Nothing
      (ImportRecord { importName = importName@(Qualified mFullModName _) } : _) -> do
        guard (not (Set.member importName exceptions))
        -- TODO need to check the whole list including provenance? eg in the
        -- case of two open imports with overlap and we're warning but it still
        -- compiles
        fullModName <- toMaybeModuleName mFullModName
        pure ((fullModName, name), localModName)

  typeExceptions :: Set (Qualified (ProperName 'TypeName))
  typeExceptions = Set.fromList
    [ Prim.Function
    , Prim.Record
    ]

  typeOpExceptions :: Set (Qualified (OpName 'TypeOpName))
  typeOpExceptions = Set.empty

  typeClassExceptions :: Set (Qualified (ProperName 'ClassName))
  typeClassExceptions = Set.fromList
    [ Prim.Fail
    , Prim.Partial
    ]

-- | This module provides functions for "re-qualifying" names in a type given
-- an Imports record, so that they can be displayed unambiguously in
-- diagnostics and suggestions without unnecessary qualification. That is,
-- given a fully qualified name like `Foo.Bar.X`, this module lets you go to
-- simply `X` (no qualification) if the given Imports record includes an open
-- import for `X` from `Foo.Bar`, or alternatively if `Foo.Bar` is imported
-- like `import Foo.Bar as F` then it produces `F.X`.
requalify :: ReverseImports -> Type a -> Type a
requalify revMap =
  everywhereOnTypes $ \ty -> fromMaybe ty $ case ty of
    TypeConstructor ann fullyQualifiedName ->
      TypeConstructor ann
        <$> reverseLookup (reverseImportsTypes revMap) fullyQualifiedName
    TypeOp ann fullyQualifiedName -> do
      TypeOp ann
        <$> reverseLookup (reverseImportsTypeOps revMap) fullyQualifiedName
    ConstrainedType ann fullyQualifiedConstraint ty' -> do
      localConstraint <- reverseLookup (reverseImportsTypeClasses revMap) (constraintClass fullyQualifiedConstraint)
      pure (ConstrainedType ann (fullyQualifiedConstraint { constraintClass = localConstraint }) ty')
    _ ->
      Nothing

requalifyConstraint :: ReverseImports -> Constraint a -> Constraint a
requalifyConstraint revMap fullyQualifiedConstraint =
  fromMaybe fullyQualifiedConstraint $ do
    localConstraint <- reverseLookup (reverseImportsTypeClasses revMap) (constraintClass fullyQualifiedConstraint)
    pure $ fullyQualifiedConstraint
      { constraintClass = localConstraint
      , constraintKindArgs = requalify revMap <$> constraintKindArgs fullyQualifiedConstraint
      , constraintArgs = requalify revMap <$> constraintArgs fullyQualifiedConstraint
      }

reverseLookup :: Show a => Ord a => ReverseImportMap a -> Qualified a -> Maybe (Qualified a)
reverseLookup revMap (Qualified mFullModName name) = do
  fullModName <- toMaybeModuleName mFullModName
  mLocalModName <- M.lookup (fullModName, name) revMap
  pure $ Qualified mLocalModName name


-- | Convert fully qualified names in errors back to unqualified or locally
-- qualified forms in certain hints and error messages, to make errors easier
-- to understand and to avoid error messages like @Could not match type Query
-- with type Query@.
requalifyTypesInErrors :: Imports -> MultipleErrors -> MultipleErrors
requalifyTypesInErrors imports =
  onErrorMessages $ \e ->
    case e of
      ErrorMessage hints (NoInstanceFound con ambig unks) ->
        ErrorMessage hints $ NoInstanceFound (requalifyConstraint revImports con) ambig unks
      _ -> onTypesInErrorMessage requal e

  where
  revImports = buildReverseImports imports
  requal = requalify revImports

--   requalifyMessage =
--     TypesDoNotUnify ty1 ty2 -> TypesDoNotUnify (requal ty1) (requal ty2)
--     KindsDoNotUnify ty1 ty2 -> KindsDoNotUnify (requal ty1) (requal ty2)
--     other -> other
--
--   requalifyHints = map $ \case
--     ErrorUnifyingTypes ty1 ty2 ->
--       ErrorUnifyingTypes (requal ty1) (requal ty2)
--     ErrorInInstance clsName tys ->
--       ErrorInInstance clsName (map requal tys)
--     ErrorInSubsumption ty1 ty2 ->
--       ErrorInSubsumption (requal ty1) (requal ty2)
--     ErrorCheckingType expr ty ->
--       ErrorCheckingType expr (requal ty)
--     ErrorCheckingKind ty1 ty2 ->
--       ErrorCheckingKind (requal ty1) (requal ty2)
--     ErrorInferringKind ty ->
--       ErrorInferringKind (requal ty)
--     ErrorInApplication expr1 ty expr2 ->
--       ErrorInApplication expr1 (requal ty) expr2
--     other ->
--       other
