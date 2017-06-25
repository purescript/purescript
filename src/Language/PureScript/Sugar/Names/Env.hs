module Language.PureScript.Sugar.Names.Env
  ( ImportRecord(..)
  , ImportProvenance(..)
  , Imports(..)
  , nullImports
  , Exports(..)
  , nullExports
  , Env
  , primEnv
  , envModuleSourceSpan
  , envModuleImports
  , envModuleExports
  , ExportMode(..)
  , exportType
  , exportTypeOp
  , exportTypeClass
  , exportValue
  , exportValueOp
  , exportKind
  , getExports
  , checkImportConflicts
  ) where

import Prelude.Compat

import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.Function (on)
import Data.Foldable (find)
import Data.List (groupBy, sortBy, delete)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.AST
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names

-- |
-- The details for an import: the name of the thing that is being imported
-- (`A.x` if importing from `A`), the module that the thing was originally
-- defined in (for re-export resolution), and the import provenance (see below).
--
data ImportRecord a =
  ImportRecord
    { importName :: Qualified a
    , importSourceModule :: ModuleName
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
  , importedKinds :: ImportMap (ProperName 'KindName)
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
    exportedTypes :: M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ModuleName)
  -- |
  -- The exported type operators along with the module they originally came
  -- from.
  --
  , exportedTypeOps :: M.Map (OpName 'TypeOpName) ModuleName
  -- |
  -- The exported classes along with the module they originally came from.
  --
  , exportedTypeClasses :: M.Map (ProperName 'ClassName) ModuleName
  -- |
  -- The exported values along with the module they originally came from.
  --
  , exportedValues :: M.Map Ident ModuleName
  -- |
  -- The exported value operators along with the module they originally came
  -- from.
  --
  , exportedValueOps :: M.Map (OpName 'ValueOpName) ModuleName
  -- |
  -- The exported kinds along with the module they originally came from.
  --
  , exportedKinds :: M.Map (ProperName 'KindName) ModuleName
  } deriving (Show)

-- |
-- An empty 'Exports' value.
--
nullExports :: Exports
nullExports = Exports M.empty M.empty M.empty M.empty M.empty M.empty

-- |
-- The imports and exports for a collection of modules. The 'SourceSpan' is used
-- to store the source location of the module with a given name, used to provide
-- useful information when there is a duplicate module definition.
--
type Env = M.Map ModuleName (SourceSpan, Imports, Exports)

-- |
-- Extracts the 'SourceSpan' from an 'Env' value.
--
envModuleSourceSpan :: (SourceSpan, a, b) -> SourceSpan
envModuleSourceSpan (ss, _, _) = ss

-- |
-- Extracts the 'Imports' from an 'Env' value.
--
envModuleImports :: (a, Imports, b) -> Imports
envModuleImports (_, imps, _) = imps

-- |
-- Extracts the 'Exports' from an 'Env' value.
--
envModuleExports :: (a, b, Exports) -> Exports
envModuleExports (_, _, exps) = exps

-- |
-- The exported types from the @Prim@ module
--
primExports :: Exports
primExports =
  nullExports
    { exportedTypes = M.fromList $ mkTypeEntry `map` M.keys primTypes
    , exportedTypeClasses = M.fromList $ mkClassEntry `map` M.keys primClasses
    , exportedKinds = M.fromList $ mkKindEntry `map` S.toList primKinds
    }
  where
  mkTypeEntry (Qualified mn name) = (name, ([], fromJust mn))
  mkClassEntry (Qualified mn name) = (name, fromJust mn)
  mkKindEntry (Qualified mn name) = (name, fromJust mn)

-- | Environment which only contains the Prim module.
primEnv :: Env
primEnv = M.singleton
  (ModuleName [ProperName "Prim"])
  (internalModuleSourceSpan "<Prim>", nullImports, primExports)

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
  => ExportMode
  -> Exports
  -> ProperName 'TypeName
  -> [ProperName 'ConstructorName]
  -> ModuleName
  -> m Exports
exportType exportMode exps name dctors mn = do
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
      forM_ (name `M.lookup` exTypes) $ \(_, mn') ->
        when (mn /= mn') $
          throwExportConflict mn mn' (TyName name)
      forM_ dctors $ \dctor ->
        forM_ ((elem dctor . fst) `find` exTypes) $ \(_, mn') ->
          when (mn /= mn') $
            throwExportConflict mn mn' (DctorName dctor)
  return $ exps { exportedTypes = M.alter updateOrInsert name exTypes }
  where
  updateOrInsert Nothing = Just (dctors, mn)
  updateOrInsert (Just (dctors', _)) = Just (dctors ++ dctors', mn)

-- |
-- Safely adds a type operator to some exports, returning an error if a
-- conflict occurs.
--
exportTypeOp
  :: MonadError MultipleErrors m
  => Exports
  -> OpName 'TypeOpName
  -> ModuleName
  -> m Exports
exportTypeOp exps op mn = do
  typeOps <- addExport TyOpName op mn (exportedTypeOps exps)
  return $ exps { exportedTypeOps = typeOps }

-- |
-- Safely adds a class to some exports, returning an error if a conflict occurs.
--
exportTypeClass
  :: MonadError MultipleErrors m
  => ExportMode
  -> Exports
  -> ProperName 'ClassName
  -> ModuleName
  -> m Exports
exportTypeClass exportMode exps name mn = do
  let exTypes = exportedTypes exps
  when (exportMode == Internal) $ do
    when (coerceProperName name `M.member` exTypes) $
      throwDeclConflict (TyClassName name) (TyName (coerceProperName name))
    when ((elem (coerceProperName name) . fst) `any` exTypes) $
      throwDeclConflict (TyClassName name) (DctorName (coerceProperName name))
  classes <- addExport TyClassName name mn (exportedTypeClasses exps)
  return $ exps { exportedTypeClasses = classes }

-- |
-- Safely adds a value to some exports, returning an error if a conflict occurs.
--
exportValue
  :: MonadError MultipleErrors m
  => Exports
  -> Ident
  -> ModuleName
  -> m Exports
exportValue exps name mn = do
  values <- addExport IdentName name mn (exportedValues exps)
  return $ exps { exportedValues = values }

-- |
-- Safely adds a value operator to some exports, returning an error if a
-- conflict occurs.
--
exportValueOp
  :: MonadError MultipleErrors m
  => Exports
  -> OpName 'ValueOpName
  -> ModuleName
  -> m Exports
exportValueOp exps op mn = do
  valueOps <- addExport ValOpName op mn (exportedValueOps exps)
  return $ exps { exportedValueOps = valueOps }

-- |
-- Safely adds a kind to some exports, returning an error if a conflict occurs.
--
exportKind
  :: MonadError MultipleErrors m
  => Exports
  -> ProperName 'KindName
  -> ModuleName
  -> m Exports
exportKind exps name mn = do
  kinds <- addExport KiName name mn (exportedKinds exps)
  return $ exps { exportedKinds = kinds }

-- |
-- Adds an entry to a list of exports unless it is already present, in which
-- case an error is returned.
--
addExport
  :: (MonadError MultipleErrors m, Ord a)
  => (a -> Name)
  -> a
  -> ModuleName
  -> M.Map a ModuleName
  -> m (M.Map a ModuleName)
addExport toName name mn exports =
  case M.lookup name exports of
    Just mn'
      | mn == mn' -> return exports
      | otherwise -> throwExportConflict mn mn' (toName name)
    Nothing ->
      return $ M.insert name mn exports

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
  => ModuleName
  -> ModuleName
  -> Name
  -> m a
throwExportConflict new existing name =
  throwError . errorMessage $
    ExportConflict (Qualified (Just new) name) (Qualified (Just existing) name)

-- |
-- Gets the exports for a module, or raise an error if the module doesn't exist.
--
getExports :: MonadError MultipleErrors m => Env -> ModuleName -> m Exports
getExports env mn =
  maybe
    (throwError . errorMessage . UnknownName . Qualified Nothing $ ModName mn)
    (return . envModuleExports)
  $ M.lookup mn env

-- |
-- When reading a value from the imports, check that there are no conflicts in
-- scope.
--
checkImportConflicts
  :: forall m a
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ModuleName
  -> (a -> Name)
  -> [ImportRecord a]
  -> m (ModuleName, ModuleName)
checkImportConflicts currentModule toName xs =
  let
    byOrig = sortBy (compare `on` importSourceModule) xs
    groups = groupBy ((==) `on` importSourceModule) byOrig
    nonImplicit = filter ((/= FromImplicit) . importProvenance) xs
    name = toName . disqualify . importName $ head xs
    conflictModules = mapMaybe (getQual . importName . head) groups
  in
    if length groups > 1
    then case nonImplicit of
      [ImportRecord (Qualified (Just mnNew) _) mnOrig _] -> do
        let warningModule = if mnNew == currentModule then Nothing else Just mnNew
        tell . errorMessage $ ScopeShadowing name warningModule $ delete mnNew conflictModules
        return (mnNew, mnOrig)
      _ -> throwError . errorMessage $ ScopeConflict name conflictModules
    else
      let ImportRecord (Qualified (Just mnNew) _) mnOrig _ = head byOrig
      in return (mnNew, mnOrig)
