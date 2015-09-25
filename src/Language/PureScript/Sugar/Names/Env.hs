-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Names.Env
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Language.PureScript.Sugar.Names.Env
  ( Imports(..)
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
  ) where

import qualified Data.Map as M

import Control.Monad
import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Environment
import Language.PureScript.Errors

-- |
-- The imported declarations for a module, including the module's own members.
--
data Imports = Imports
  {
  -- |
  -- Local names for types within a module mapped to to their qualified names
  --
    importedTypes :: M.Map (Qualified ProperName) (Qualified ProperName, ModuleName)
  -- |
  -- Local names for data constructors within a module mapped to to their qualified names
  --
  , importedDataConstructors :: M.Map (Qualified ProperName) (Qualified ProperName, ModuleName)
  -- |
  -- Local names for classes within a module mapped to to their qualified names
  --
  , importedTypeClasses :: M.Map (Qualified ProperName) (Qualified ProperName, ModuleName)
  -- |
  -- Local names for values within a module mapped to to their qualified names
  --
  , importedValues :: M.Map (Qualified Ident) (Qualified Ident, ModuleName)
  -- |
  -- The list of modules that have been imported into the current scope.
  --
  , importedModules :: [ModuleName]
  } deriving (Show, Read)

-- |
-- An empty 'Imports' value.
--
nullImports :: Imports
nullImports = Imports M.empty M.empty M.empty M.empty []

-- |
-- The exported declarations from a module.
--
data Exports = Exports
  {
  -- |
  -- The types exported from each module along with the module they originally
  -- came from.
  --
    exportedTypes :: [((ProperName, [ProperName]), ModuleName)]
  -- |
  -- The classes exported from each module along with the module they originally
  -- came from.
  --
  , exportedTypeClasses :: [(ProperName, ModuleName)]
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
primExports = Exports (mkTypeEntry `map` M.keys primTypes) [] []
  where
  mkTypeEntry (Qualified _ name) = ((name, []), ModuleName [ProperName "Prim"])

-- | Environment which only contains the Prim module.
primEnv :: Env
primEnv = M.singleton
  (ModuleName [ProperName "Prim"])
  (internalModuleSourceSpan "<Prim>", nullImports, primExports)

-- |
-- Safely adds a type and its data constructors to some exports, returning an
-- error if a conflict occurs.
--
exportType :: (MonadError MultipleErrors m) => Exports -> ProperName -> [ProperName] -> ModuleName -> m Exports
exportType exps name dctors mn = do
  let exTypes = exportedTypes exps
  let exDctors = (snd . fst) `concatMap` exTypes
  let exClasses = exportedTypeClasses exps
  when (any (\((name', _), _) -> name == name') exTypes) $ throwConflictError ConflictingTypeDecls name
  when (any ((== name) . fst) exClasses) $ throwConflictError TypeConflictsWithClass name
  forM_ dctors $ \dctor -> do
    when (dctor `elem` exDctors) $ throwConflictError ConflictingCtorDecls dctor
    when (any ((== dctor) . fst) exClasses) $ throwConflictError CtorConflictsWithClass dctor
  return $ exps { exportedTypes = ((name, dctors), mn) : exTypes }

-- |
-- Safely adds a class to some exports, returning an error if a conflict occurs.
--
exportTypeClass :: (MonadError MultipleErrors m) => Exports -> ProperName -> ModuleName -> m Exports
exportTypeClass exps name mn = do
  let exTypes = exportedTypes exps
  let exDctors = (snd . fst) `concatMap` exTypes
  when (any (\((name', _), _) -> name == name') exTypes) $ throwConflictError ClassConflictsWithType name
  when (name `elem` exDctors) $ throwConflictError ClassConflictsWithCtor name
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
  if any ((== name) . fst) exports
  then throwConflictError what name
  else return $ (name, mn) : exports

-- |
-- Raises an error for when there is more than one definition for something.
--
throwConflictError :: (MonadError MultipleErrors m) => (a -> SimpleErrorMessage) -> a -> m b
throwConflictError conflict = throwError . errorMessage . conflict
