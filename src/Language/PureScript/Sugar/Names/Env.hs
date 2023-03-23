module Language.PureScript.Sugar.Names.Env
  ( ImportRecord(..)
  , ImportProvenance(..)
  , Imports(..)
  , nullImports
  , Exports(..)
  , nullExports
  , Env
  , primEnv
  , primExports
  , envModuleExports
  , ExportMode(..)
  , exportType
  , exportTypeOp
  , exportTypeClass
  , exportValue
  , exportValueOp
  , checkImportConflicts
  ) where

import Prelude

import Control.Monad (forM_, when)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.Function (on)
import Data.Foldable (find)
import Data.List (groupBy, sortOn, delete)
import Data.Maybe (mapMaybe)
import Safe (headMay)
import Data.Map qualified as M
import Data.Set qualified as S

import Language.PureScript.Constants.Prim qualified as C
import Language.PureScript.AST (ExportSource(..), SourceSpan, internalModuleSourceSpan, nullSourceSpan)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), errorMessage, errorMessage')
import Language.PureScript.Names (Ident, ModuleName, Name(..), OpName, OpNameType(..), ProperName, ProperNameType(..), Qualified(..), QualifiedBy(..), coerceProperName, disqualify, getQual)

-- |
-- The details for an import: the name of the thing that is being imported
-- (`A.x` if importing from `A`), the module that the thing was originally
-- defined in (for re-export resolution), and the import provenance (see below).
--
data ImportRecord a =
  ImportRecord
    { importName :: Qualified a
    , importSourceModule :: ModuleName
    , importSourceSpan :: SourceSpan
    , importProvenance :: ImportProvenance
    }
    deriving (Eq, Ord, Show)

-- |
-- Used to track how an import was introduced into scope. This allows us to
-- handle the one-open-import special case that allows a name conflict to become
-- a warning rather than being an unresolvable situation.
--
data ImportProvenance
  = FromImplicit
  | FromExplicit
  | Local
  | Prim
  deriving (Eq, Ord, Show)

type ImportMap a = M.Map (Qualified a) [ImportRecord a]

-- |
-- The imported declarations for a module, including the module's own members.
--
data Imports = Imports
  {
  -- |
  -- Local names for types within a module mapped to their qualified names
  --
    importedTypes :: ImportMap (ProperName 'TypeName)
  -- |
  -- Local names for type operators within a module mapped to their qualified names
  --
  , importedTypeOps :: ImportMap (OpName 'TypeOpName)
  -- |
  -- Local names for data constructors within a module mapped to their qualified names
  --
  , importedDataConstructors :: ImportMap (ProperName 'ConstructorName)
  -- |
  -- Local names for classes within a module mapped to their qualified names
  --
  , importedTypeClasses :: ImportMap (ProperName 'ClassName)
  -- |
  -- Local names for values within a module mapped to their qualified names
  --
  , importedValues :: ImportMap Ident
  -- |
  -- Local names for value operators within a module mapped to their qualified names
  --
  , importedValueOps :: ImportMap (OpName 'ValueOpName)
  -- |
  -- The name of modules that have been imported into the current scope that
  -- can be re-exported. If a module is imported with `as` qualification, the
  -- `as` name appears here, otherwise the original name.
  --
  , importedModules :: S.Set ModuleName
  -- |
  -- The "as" names of modules that have been imported qualified.
  --
  , importedQualModules :: S.Set ModuleName
  -- |
  -- Local names for kinds within a module mapped to their qualified names
  --
  , importedKinds :: ImportMap (ProperName 'TypeName)
  } deriving (Show)

nullImports :: Imports
nullImports = Imports M.empty M.empty M.empty M.empty M.empty M.empty S.empty S.empty M.empty

-- |
-- The exported declarations from a module.
--
data Exports = Exports
  {
  -- |
  -- The exported types along with the module they originally came from.
  --
    exportedTypes :: M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ExportSource)
  -- |
  -- The exported type operators along with the module they originally came
  -- from.
  --
  , exportedTypeOps :: M.Map (OpName 'TypeOpName) ExportSource
  -- |
  -- The exported classes along with the module they originally came from.
  --
  , exportedTypeClasses :: M.Map (ProperName 'ClassName) ExportSource
  -- |
  -- The exported values along with the module they originally came from.
  --
  , exportedValues :: M.Map Ident ExportSource
  -- |
  -- The exported value operators along with the module they originally came
  -- from.
  --
  , exportedValueOps :: M.Map (OpName 'ValueOpName) ExportSource
  } deriving (Show)

-- |
-- An empty 'Exports' value.
--
nullExports :: Exports
nullExports = Exports M.empty M.empty M.empty M.empty M.empty

-- |
-- The imports and exports for a collection of modules. The 'SourceSpan' is used
-- to store the source location of the module with a given name, used to provide
-- useful information when there is a duplicate module definition.
--
type Env = M.Map ModuleName (SourceSpan, Imports, Exports)

-- |
-- Extracts the 'Exports' from an 'Env' value.
--
envModuleExports :: (a, b, Exports) -> Exports
envModuleExports (_, _, exps) = exps

-- |
-- The exported types from the @Prim@ module
--
primExports :: Exports
primExports = mkPrimExports primTypes primClasses

-- |
-- The exported types from the @Prim.Boolean@ module
--
primBooleanExports :: Exports
primBooleanExports = mkPrimExports primBooleanTypes mempty

-- |
-- The exported types from the @Prim.Coerce@ module
--
primCoerceExports :: Exports
primCoerceExports = mkPrimExports primCoerceTypes primCoerceClasses

-- |
-- The exported types from the @Prim.Ordering@ module
--
primOrderingExports :: Exports
primOrderingExports = mkPrimExports primOrderingTypes mempty

-- |
-- The exported types from the @Prim.Row@ module
--
primRowExports :: Exports
primRowExports = mkPrimExports primRowTypes primRowClasses

-- |
-- The exported types from the @Prim.RowList@ module
--
primRowListExports :: Exports
primRowListExports = mkPrimExports primRowListTypes primRowListClasses

-- |
-- The exported types from the @Prim.Symbol@ module
--
primSymbolExports :: Exports
primSymbolExports = mkPrimExports primSymbolTypes primSymbolClasses

-- |
-- The exported types from the @Prim.Int@ module
primIntExports :: Exports
primIntExports = mkPrimExports primIntTypes primIntClasses

-- |
-- The exported types from the @Prim.TypeError@ module
--
primTypeErrorExports :: Exports
primTypeErrorExports = mkPrimExports primTypeErrorTypes primTypeErrorClasses

-- |
-- Create a set of exports for a Prim module.
--
mkPrimExports
  :: M.Map (Qualified (ProperName 'TypeName)) a
  -> M.Map (Qualified (ProperName 'ClassName)) b
  -> Exports
mkPrimExports ts cs =
  nullExports
    { exportedTypes = M.fromList $ mkTypeEntry `map` M.keys ts
    , exportedTypeClasses = M.fromList $ mkClassEntry `map` M.keys cs
    }
  where
  mkTypeEntry (Qualified (ByModuleName mn) name) = (name, ([], primExportSource mn))
  mkTypeEntry _ = internalError
    "mkPrimExports.mkTypeEntry: a name is qualified BySourcePos instead of ByModuleName"

  mkClassEntry (Qualified (ByModuleName mn) name) = (name, primExportSource mn)
  mkClassEntry _ = internalError
    "mkPrimExports.mkClassEntry: a name is qualified BySourcePos instead of ByModuleName"

  primExportSource mn =
    ExportSource
      { exportSourceImportedFrom = Nothing
      , exportSourceDefinedIn = mn
      }

-- | Environment which only contains the Prim modules.
primEnv :: Env
primEnv = M.fromList
  [ ( C.M_Prim
    , (internalModuleSourceSpan "<Prim>", nullImports, primExports)
    )
  , ( C.M_Prim_Boolean
    , (internalModuleSourceSpan "<Prim.Boolean>", nullImports, primBooleanExports)
    )
  , ( C.M_Prim_Coerce
    , (internalModuleSourceSpan "<Prim.Coerce>", nullImports, primCoerceExports)
    )
  , ( C.M_Prim_Ordering
    , (internalModuleSourceSpan "<Prim.Ordering>", nullImports, primOrderingExports)
    )
  , ( C.M_Prim_Row
    , (internalModuleSourceSpan "<Prim.Row>", nullImports, primRowExports)
    )
  , ( C.M_Prim_RowList
    , (internalModuleSourceSpan "<Prim.RowList>", nullImports, primRowListExports)
    )
  , ( C.M_Prim_Symbol
    , (internalModuleSourceSpan "<Prim.Symbol>", nullImports, primSymbolExports)
    )
  , ( C.M_Prim_Int
    , (internalModuleSourceSpan "<Prim.Int>", nullImports, primIntExports)
    )
  , ( C.M_Prim_TypeError
    , (internalModuleSourceSpan "<Prim.TypeError>", nullImports, primTypeErrorExports)
    )
  ]

-- |
-- When updating the `Exports` the behaviour is slightly different depending
-- on whether we are exporting values defined within the module or elaborating
-- re-exported values. This type is used to indicate which behaviour should be
-- used.
--
data ExportMode = Internal | ReExport
  deriving (Eq, Show)

-- |
-- Safely adds a type and its data constructors to some exports, returning an
-- error if a conflict occurs.
--
exportType
  :: MonadError MultipleErrors m
  => SourceSpan
  -> ExportMode
  -> Exports
  -> ProperName 'TypeName
  -> [ProperName 'ConstructorName]
  -> ExportSource
  -> m Exports
exportType ss exportMode exps name dctors src = do
  let exTypes = exportedTypes exps
      exClasses = exportedTypeClasses exps
      dctorNameCounts :: [(ProperName 'ConstructorName, Int)]
      dctorNameCounts = M.toList $ M.fromListWith (+) (map (,1) dctors)
  forM_ dctorNameCounts $ \(dctorName, count) ->
    when (count > 1) $
      throwDeclConflict (DctorName dctorName) (DctorName dctorName)
  case exportMode of
    Internal -> do
      when (name `M.member` exTypes) $
        throwDeclConflict (TyName name) (TyName name)
      when (coerceProperName name `M.member` exClasses) $
        throwDeclConflict (TyName name) (TyClassName (coerceProperName name))
      forM_ dctors $ \dctor -> do
        when ((elem dctor . fst) `any` exTypes) $
          throwDeclConflict (DctorName dctor) (DctorName dctor)
        when (coerceProperName dctor `M.member` exClasses) $
          throwDeclConflict (DctorName dctor) (TyClassName (coerceProperName dctor))
    ReExport -> do
      let mn = exportSourceDefinedIn src
      forM_ (coerceProperName name `M.lookup` exClasses) $ \src' ->
        let mn' = exportSourceDefinedIn src' in
        throwExportConflict' ss mn mn' (TyName name) (TyClassName (coerceProperName name))
      forM_ (name `M.lookup` exTypes) $ \(_, src') ->
        let mn' = exportSourceDefinedIn src' in
        when (mn /= mn') $
          throwExportConflict ss mn mn' (TyName name)
      forM_ dctors $ \dctor ->
        forM_ ((elem dctor . fst) `find` exTypes) $ \(_, src') ->
          let mn' = exportSourceDefinedIn src' in
          when (mn /= mn') $
            throwExportConflict ss mn mn' (DctorName dctor)
  return $ exps { exportedTypes = M.alter updateOrInsert name exTypes }
  where
  updateOrInsert Nothing = Just (dctors, src)
  updateOrInsert (Just (dctors', _)) = Just (dctors ++ dctors', src)

-- |
-- Safely adds a type operator to some exports, returning an error if a
-- conflict occurs.
--
exportTypeOp
  :: MonadError MultipleErrors m
  => SourceSpan
  -> Exports
  -> OpName 'TypeOpName
  -> ExportSource
  -> m Exports
exportTypeOp ss exps op src = do
  typeOps <- addExport ss TyOpName op src (exportedTypeOps exps)
  return $ exps { exportedTypeOps = typeOps }

-- |
-- Safely adds a class to some exports, returning an error if a conflict occurs.
--
exportTypeClass
  :: MonadError MultipleErrors m
  => SourceSpan
  -> ExportMode
  -> Exports
  -> ProperName 'ClassName
  -> ExportSource
  -> m Exports
exportTypeClass ss exportMode exps name src = do
  let exTypes = exportedTypes exps
  when (exportMode == Internal) $ do
    when (coerceProperName name `M.member` exTypes) $
      throwDeclConflict (TyClassName name) (TyName (coerceProperName name))
    when ((elem (coerceProperName name) . fst) `any` exTypes) $
      throwDeclConflict (TyClassName name) (DctorName (coerceProperName name))
  classes <- addExport ss TyClassName name src (exportedTypeClasses exps)
  return $ exps { exportedTypeClasses = classes }

-- |
-- Safely adds a value to some exports, returning an error if a conflict occurs.
--
exportValue
  :: MonadError MultipleErrors m
  => SourceSpan
  -> Exports
  -> Ident
  -> ExportSource
  -> m Exports
exportValue ss exps name src = do
  values <- addExport ss IdentName name src (exportedValues exps)
  return $ exps { exportedValues = values }

-- |
-- Safely adds a value operator to some exports, returning an error if a
-- conflict occurs.
--
exportValueOp
  :: MonadError MultipleErrors m
  => SourceSpan
  -> Exports
  -> OpName 'ValueOpName
  -> ExportSource
  -> m Exports
exportValueOp ss exps op src = do
  valueOps <- addExport ss ValOpName op src (exportedValueOps exps)
  return $ exps { exportedValueOps = valueOps }

-- |
-- Adds an entry to a list of exports unless it is already present, in which
-- case an error is returned.
--
addExport
  :: (MonadError MultipleErrors m, Ord a)
  => SourceSpan
  -> (a -> Name)
  -> a
  -> ExportSource
  -> M.Map a ExportSource
  -> m (M.Map a ExportSource)
addExport ss toName name src exports =
  case M.lookup name exports of
    Just src' ->
      let
        mn = exportSourceDefinedIn src
        mn' = exportSourceDefinedIn src'
      in
        if mn == mn'
          then return exports
          else throwExportConflict ss mn mn' (toName name)
    Nothing ->
      return $ M.insert name src exports

-- |
-- Raises an error for when there is more than one definition for something.
--
throwDeclConflict
  :: MonadError MultipleErrors m
  => Name
  -> Name
  -> m a
throwDeclConflict new existing =
  throwError . errorMessage $ DeclConflict new existing

-- |
-- Raises an error for when there are conflicting names in the exports.
--
throwExportConflict
  :: MonadError MultipleErrors m
  => SourceSpan
  -> ModuleName
  -> ModuleName
  -> Name
  -> m a
throwExportConflict ss new existing name =
  throwExportConflict' ss new existing name name

-- |
-- Raises an error for when there are conflicting names in the exports. Allows
-- different categories of names. E.g. class and type names conflicting.
--
throwExportConflict'
  :: MonadError MultipleErrors m
  => SourceSpan
  -> ModuleName
  -> ModuleName
  -> Name
  -> Name
  -> m a
throwExportConflict' ss new existing newName existingName =
  throwError . errorMessage' ss $
    ExportConflict (Qualified (ByModuleName new) newName) (Qualified (ByModuleName existing) existingName)

-- |
-- When reading a value from the imports, check that there are no conflicts in
-- scope.
--
checkImportConflicts
  :: forall m a
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => SourceSpan
  -> ModuleName
  -> (a -> Name)
  -> [ImportRecord a]
  -> m (ModuleName, ModuleName)
checkImportConflicts ss currentModule toName xs =
  let
    byOrig = sortOn importSourceModule xs
    groups = groupBy ((==) `on` importSourceModule) byOrig
    nonImplicit = filter ((/= FromImplicit) . importProvenance) xs
    name = toName . disqualify . importName $ head xs
    conflictModules = mapMaybe (getQual . importName . head) groups
  in
    if length groups > 1
    then case nonImplicit of
      [ImportRecord (Qualified (ByModuleName mnNew) _) mnOrig _ _] -> do
        let warningModule = if mnNew == currentModule then Nothing else Just mnNew
            ss' = maybe nullSourceSpan importSourceSpan . headMay . filter ((== FromImplicit) . importProvenance) $ xs
        tell . errorMessage' ss' $ ScopeShadowing name warningModule $ delete mnNew conflictModules
        return (mnNew, mnOrig)
      _ -> throwError . errorMessage' ss $ ScopeConflict name conflictModules
    else
      case head byOrig of
        ImportRecord (Qualified (ByModuleName mnNew) _) mnOrig _ _ ->
          return (mnNew, mnOrig)
        _ ->
          internalError "checkImportConflicts: ImportRecord should be qualified"
