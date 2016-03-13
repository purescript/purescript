{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , exportType
  , exportTypeClass
  , exportValue
  , getExports
  , checkImportConflicts
  ) where

import Data.Function (on)
import Data.List (groupBy, sortBy, nub, delete)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..))

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Names
import Language.PureScript.Environment
import Language.PureScript.Errors

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
    deriving (Eq, Ord, Show, Read)

-- |
-- Used to track how an import was introduced into scope. This allows us to
-- handle the one-open-import special case that allows a name conflict to become
-- a warning rather than being an unresolvable situation.
--
data ImportProvenance
  = FromImplicit
  | FromExplicit
  | Local
  deriving (Eq, Ord, Show, Read)

-- |
-- The imported declarations for a module, including the module's own members.
--
data Imports = Imports
  {
  -- |
  -- Local names for types within a module mapped to to their qualified names
  --
    importedTypes :: M.Map (Qualified (ProperName 'TypeName)) [ImportRecord (ProperName 'TypeName)]
  -- |
  -- Local names for data constructors within a module mapped to to their qualified names
  --
  , importedDataConstructors :: M.Map (Qualified (ProperName 'ConstructorName)) [ImportRecord (ProperName 'ConstructorName)]
  -- |
  -- Local names for classes within a module mapped to to their qualified names
  --
  , importedTypeClasses :: M.Map (Qualified (ProperName 'ClassName)) [ImportRecord (ProperName 'ClassName)]
  -- |
  -- Local names for values within a module mapped to to their qualified names
  --
  , importedValues :: M.Map (Qualified Ident) [ImportRecord Ident]
  -- |
  -- The modules that have been imported into the current scope.
  --
  , importedModules :: S.Set ModuleName
  -- |
  -- The names of "virtual" modules that come into existence when "import as"
  -- is used.
  --
  , importedVirtualModules :: S.Set ModuleName
  } deriving (Show, Read)

-- |
-- An empty 'Imports' value.
--
nullImports :: Imports
nullImports = Imports M.empty M.empty M.empty M.empty S.empty S.empty

-- |
-- The exported declarations from a module.
--
data Exports = Exports
  {
  -- |
  -- The types exported from each module along with the module they originally
  -- came from.
  --
    exportedTypes :: [((ProperName 'TypeName, [ProperName 'ConstructorName]), ModuleName)]
  -- |
  -- The classes exported from each module along with the module they originally
  -- came from.
  --
  , exportedTypeClasses :: [(ProperName 'ClassName, ModuleName)]
  -- |
  -- The values exported from each module along with the module they originally
  -- came from.
  --
  , exportedValues :: [(Ident, ModuleName)]
  } deriving (Show, Read)

-- |
-- An empty 'Exports' value.
--
nullExports :: Exports
nullExports = Exports [] [] []

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
primExports = Exports (mkTypeEntry `map` M.keys primTypes) (mkClassEntry `map` M.keys primClasses) []
  where
  mkTypeEntry (Qualified mn name) = ((name, []), fromJust mn)
  mkClassEntry (Qualified mn name) = (name, fromJust mn)

-- | Environment which only contains the Prim module.
primEnv :: Env
primEnv = M.singleton
  (ModuleName [ProperName "Prim"])
  (internalModuleSourceSpan "<Prim>", nullImports, primExports)

-- |
-- Safely adds a type and its data constructors to some exports, returning an
-- error if a conflict occurs.
--
exportType :: (MonadError MultipleErrors m) => Exports -> ProperName 'TypeName -> [ProperName 'ConstructorName] -> ModuleName -> m Exports
exportType exps name dctors mn = do
  let exTypes' = exportedTypes exps
  let exTypes = filter ((/= mn) . snd) exTypes'
  let exDctors = (snd . fst) `concatMap` exTypes
  let exClasses = exportedTypeClasses exps
  when (any ((== name) . fst . fst) exTypes) $ throwConflictError ConflictingTypeDecls name
  when (any ((== coerceProperName name) . fst) exClasses) $ throwConflictError TypeConflictsWithClass name
  forM_ dctors $ \dctor -> do
    when (dctor `elem` exDctors) $ throwConflictError ConflictingCtorDecls dctor
    when (any ((== coerceProperName dctor) . fst) exClasses) $ throwConflictError CtorConflictsWithClass dctor
  return $ exps { exportedTypes = nub $ ((name, dctors), mn) : exTypes' }

-- |
-- Safely adds a class to some exports, returning an error if a conflict occurs.
--
exportTypeClass :: (MonadError MultipleErrors m) => Exports -> ProperName 'ClassName -> ModuleName -> m Exports
exportTypeClass exps name mn = do
  let exTypes = exportedTypes exps
  let exDctors = (snd . fst) `concatMap` exTypes
  when (any ((== coerceProperName name) . fst . fst) exTypes) $ throwConflictError ClassConflictsWithType name
  when (coerceProperName name `elem` exDctors) $ throwConflictError ClassConflictsWithCtor name
  classes <- addExport DuplicateClassExport name mn (exportedTypeClasses exps)
  return $ exps { exportedTypeClasses = classes }

-- |
-- Safely adds a value to some exports, returning an error if a conflict occurs.
--
exportValue :: (MonadError MultipleErrors m) => Exports -> Ident -> ModuleName -> m Exports
exportValue exps name mn = do
  values <- addExport DuplicateValueExport name mn (exportedValues exps)
  return $ exps { exportedValues = values }

-- |
-- Adds an entry to a list of exports unless it is already present, in which case an error is
-- returned.
--
addExport :: (MonadError MultipleErrors m, Eq a) => (a -> SimpleErrorMessage) -> a -> ModuleName -> [(a, ModuleName)] -> m [(a, ModuleName)]
addExport what name mn exports =
  if any (\(name', mn') -> name == name' && mn /= mn') exports
  then throwConflictError what name
  else return $ nub $ (name, mn) : exports

-- |
-- Raises an error for when there is more than one definition for something.
--
throwConflictError :: (MonadError MultipleErrors m) => (a -> SimpleErrorMessage) -> a -> m b
throwConflictError conflict = throwError . errorMessage . conflict

-- Gets the exports for a module, or an error message if the module doesn't exist
getExports :: (MonadError MultipleErrors m) => Env -> ModuleName -> m Exports
getExports env mn = maybe (throwError . errorMessage $ UnknownModule mn) (return . envModuleExports) $ M.lookup mn env

-- |
-- When reading a value from the imports, check that there are no conflicts in
-- scope.
--
checkImportConflicts
  :: forall m a
   . (Show a, MonadError MultipleErrors m, MonadWriter MultipleErrors m, Ord a)
  => ModuleName
  -> (a -> String)
  -> [ImportRecord a]
  -> m (ModuleName, ModuleName)
checkImportConflicts currentModule render xs =
  let
    byOrig = sortBy (compare `on` importSourceModule) xs
    groups = groupBy ((==) `on` importSourceModule) byOrig
    nonImplicit = filter ((/= FromImplicit) . importProvenance) xs
    name = render' (importName . head $ xs)
    conflictModules = map (getQual . importName . head) groups
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
  where
  getQual :: Qualified a -> ModuleName
  getQual (Qualified (Just mn) _) = mn
  getQual _ = internalError "unexpected unqualified name in checkImportConflicts"
  render' :: Qualified a -> String
  render' (Qualified _ a) = render a
