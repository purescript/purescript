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
  }
  deriving (Show)

buildReverseImports :: Imports -> ReverseImports
buildReverseImports Imports { importedTypes, importedTypeOps } =
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
    }

  preferShortest :: QualifiedBy -> QualifiedBy -> QualifiedBy
  preferShortest (ByModuleName x) (ByModuleName y) = do
    ByModuleName $
      if T.length (runModuleName x) < T.length (runModuleName y)
        then x
        else y
  preferShortest _ y = y

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
    , fmap coerceProperName Prim.Fail
    ]

  typeOpExceptions :: Set (Qualified (OpName 'TypeOpName))
  typeOpExceptions = Set.empty

-- | This module provides functions for "re-qualifying" names in a type given
-- an Imports record, so that they can be displayed unambiguously in
-- diagnostics and suggestions without unnecessary qualification. That is,
-- given a fully qualified name like `Foo.Bar.X`, this module lets you go to
-- simply `X` (no qualification) if the given Imports record includes an open
-- import for `X` from `Foo.Bar`, or alternatively if `Foo.Bar` is imported
-- like `import Foo.Bar as F` then it produces `F.X`.
requalify :: ReverseImports -> Type a -> Type a
requalify ReverseImports { reverseImportsTypes, reverseImportsTypeOps } =
  everywhereOnTypes $ \ty -> fromMaybe ty $ case ty of
    TypeConstructor ann fullyQualifiedName ->
      fmap (TypeConstructor ann) $
        reverseLookup reverseImportsTypes fullyQualifiedName
    TypeOp ann fullyQualifiedName -> do
      fmap (TypeOp ann) $
        reverseLookup reverseImportsTypeOps fullyQualifiedName
    _ ->
      Nothing
  where
  reverseLookup :: Ord a => ReverseImportMap a -> Qualified a -> Maybe (Qualified a)
  reverseLookup revMap (Qualified mFullModName name) = do
    fullModName <- toMaybeModuleName mFullModName
    mLocalModName <- M.lookup (fullModName, name) revMap
    pure $ Qualified mLocalModName name
