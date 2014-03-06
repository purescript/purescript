-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Names
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.Names (
  desugarImports
) where

import Data.Maybe (fromMaybe)
import Data.Generics (extM, mkM, everywhereM)
import Data.Generics.Extras (mkS, extS, everywhereWithContextM')

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Monad.Error

import qualified Data.Map as M

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Values
import Language.PureScript.Prim

-- |
-- The global export environment - every declaration exported from every module.
--
type ExportEnvironment = M.Map ModuleName Exports

-- |
-- The exported declarations from a module.
--
data Exports = Exports
  -- |
  -- The types exported from each module
  --
  { exportedTypes :: [(ProperName, [ProperName])]
  -- |
  -- The classes exported from each module
  --
  , exportedTypeClasses :: [ProperName]
  -- |
  -- The values exported from each module
  , exportedValues :: [Ident]
  --
  } deriving (Show)

-- |
-- An imported environment for a particular module. This also contains the module's own members.
--
data ImportEnvironment = ImportEnvironment
  -- |
  -- Local names for types within a module mapped to to their qualified names
  --
  { importedTypes :: M.Map ProperName (Qualified ProperName)
  -- |
  -- Local names for data constructors within a module mapped to to their qualified names
  --
  , importedDataConstructors :: M.Map ProperName (Qualified ProperName)
  -- |
  -- Local names for classes within a module mapped to to their qualified names
  --
  , importedTypeClasses :: M.Map ProperName (Qualified ProperName)
  -- |
  -- Local names for values within a module mapped to to their qualified names
  --
  , importedValues :: M.Map Ident (Qualified Ident)
  } deriving (Show)

-- |
-- Updates the exports for a module from the global environment. If the module was not previously
-- present in the global environment, it is created.
--
updateExportedModule :: ExportEnvironment -> ModuleName -> (Exports -> Either String Exports) -> Either String ExportEnvironment
updateExportedModule env mn update = do
  let exports = fromMaybe (error "Module was undefined in updateExportedModule") $ mn `M.lookup` env
  exports' <- update exports
  return $ M.insert mn exports' env

-- |
-- Adds an empty module to an ExportEnvironment.
--
addEmptyModule :: ExportEnvironment -> ModuleName -> ExportEnvironment
addEmptyModule env name = M.insert name (Exports [] [] []) env

-- |
-- Adds a type belonging to a module to the export environment.
--
addType :: ExportEnvironment -> ModuleName -> ProperName -> [ProperName] -> Either String ExportEnvironment
addType env mn name dctors = updateExportedModule env mn $ \m -> do
  types <- addExport (exportedTypes m) (name, dctors)
  return $ m { exportedTypes = types }

-- |
-- Adds a class to the export environment.
--
addTypeClass :: ExportEnvironment -> ModuleName -> ProperName -> Either String ExportEnvironment
addTypeClass env mn name = updateExportedModule env mn $ \m -> do
  classes <- addExport (exportedTypeClasses m) name
  return $ m { exportedTypeClasses = classes }

-- |
-- Adds a class to the export environment.
--
addValue :: ExportEnvironment -> ModuleName -> Ident -> Either String ExportEnvironment
addValue env mn name = updateExportedModule env mn $ \m -> do
  values <- addExport (exportedValues m) name
  return $ m { exportedValues = values }

-- |
-- Adds an entry to a list of exports unless it is already present, in which case an error is
-- returned.
--
addExport :: (Eq a, Show a) => [a] -> a -> Either String [a]
addExport exports name =
  if name `elem` exports
  then throwError $ "Multiple definitions for '" ++ show name ++ "'"
  else return $ name : exports

-- |
-- Replaces all local names with qualified names within a set of modules.
--
desugarImports :: [Module] -> Either String [Module]
desugarImports modules = do
  unfilteredExports <- findExports modules
  exports <- foldM filterModuleExports unfilteredExports modules
  mapM (renameInModule' unfilteredExports exports) modules
  where

  -- Filters the exports for a module in the global exports environment so that only explicitly
  -- exported members remain. If the module does not explicitly export anything, everything is
  -- exported.
  filterModuleExports :: ExportEnvironment -> Module -> Either String ExportEnvironment
  filterModuleExports env (Module mn _ (Just exps)) = filterExports mn exps env
  filterModuleExports env _ = return env

  -- Rename and check all the names within a module. We tweak the global exports environment so
  -- the module has access to an unfiltered list of its own members.
  renameInModule' :: ExportEnvironment -> ExportEnvironment -> Module -> Either String Module
  renameInModule' unfilteredExports exports m@(Module mn _ _) = rethrowForModule m $ do
    let exports' = M.update (\_ -> M.lookup mn unfilteredExports) mn exports
    imports <- resolveImports exports' m
    renameInModule imports exports' m

-- |
-- Rethrow an error with the name of the current module in the case of a failure
--
rethrowForModule :: Module -> Either String a -> Either String a
rethrowForModule (Module mn _ _) = flip catchError $ \e -> throwError ("Error in module '" ++ show mn ++ "':\n" ++  e)

-- |
-- Replaces all local names with qualified names within a module and checks that all existing
-- qualified names are valid.
--
renameInModule :: ImportEnvironment -> ExportEnvironment -> Module -> Either String Module
renameInModule imports exports (Module mn decls exps) =
  Module mn <$> (mapM updateDecl decls >>= everywhereM (mkM updateType `extM` updateValue `extM` updateBinder `extM` updateVars)) <*> pure exps
  where
  updateDecl (TypeInstanceDeclaration name cs cn ts ds) =
      TypeInstanceDeclaration name <$> updateConstraints cs <*> updateClassName cn <*> pure ts <*> pure ds
  updateDecl d = return d

  updateVars :: Declaration -> Either String Declaration
  updateVars (ValueDeclaration name [] Nothing val) =
    ValueDeclaration name [] Nothing <$> everywhereWithContextM' [] (mkS bindFunctionArgs `extS` bindBinders) val
    where
    bindFunctionArgs bound (Abs (Left arg) val') = return (arg : bound, Abs (Left arg) val')
    bindFunctionArgs bound (Var name'@(Qualified Nothing ident)) | ident `notElem` bound = (,) bound <$> (Var <$> updateValueName name')
    bindFunctionArgs bound (Var name'@(Qualified (Just _) _)) = (,) bound <$> (Var <$> updateValueName name')
    bindFunctionArgs bound other = return (bound, other)
    bindBinders :: [Ident] -> CaseAlternative -> Either String ([Ident], CaseAlternative)
    bindBinders bound c@(CaseAlternative bs _ _) = return (binderNames bs ++ bound, c)
  updateVars (ValueDeclaration name _ _ _) = error $ "Binders should have been desugared in " ++ show name
  updateVars other = return other
  updateValue (Constructor name) = Constructor <$> updateDataConstructorName name
  updateValue v = return v
  updateBinder (ConstructorBinder name b) = ConstructorBinder <$> updateDataConstructorName name <*> pure b
  updateBinder v = return v
  updateType (TypeConstructor name) = TypeConstructor <$> updateTypeName name
  updateType (SaturatedTypeSynonym name tys) = SaturatedTypeSynonym <$> updateTypeName name <*> mapM updateType tys
  updateType (ConstrainedType cs t) = ConstrainedType <$> updateConstraints cs <*> pure t
  updateType t = return t
  updateConstraints = mapM (\(name, ts) -> (,) <$> updateClassName name <*> pure ts)

  updateTypeName (Qualified Nothing name) = update "type" importedTypes name
  updateTypeName (Qualified (Just mn') name) = do
    modExports <- getExports mn'
    case name `lookup` exportedTypes modExports of
      Nothing -> throwError $ "Unknown type '" ++ show (Qualified (Just mn') name) ++ "'"
      _ -> return $ Qualified (Just mn') name

  updateDataConstructorName (Qualified Nothing name) = update "data constructor" importedDataConstructors name
  updateDataConstructorName (Qualified (Just mn') name) = do
    modExports <- getExports mn'
    let allDcons = join $ snd `map` exportedTypes modExports
    if name `elem` allDcons
      then return $ Qualified (Just mn') name
      else throwError $ "Unknown data constructor '" ++ show (Qualified (Just mn') name) ++ "'"

  updateClassName (Qualified Nothing name) = update "type class" importedTypeClasses name
  updateClassName (Qualified (Just mn') name) = check "type class" exportedTypeClasses mn' name

  updateValueName (Qualified Nothing name) = update "value" importedValues name
  updateValueName (Qualified (Just mn') name) = check "value" exportedValues mn' name

  -- Replace an unqualified name with a qualified
  update :: (Ord a, Show a) => String -> (ImportEnvironment -> M.Map a (Qualified a)) -> a -> Either String (Qualified a)
  update t get name = maybe (throwError $ "Unknown " ++ t ++ " '" ++ show name ++ "'") return $ M.lookup name (get imports)

  -- Check that a qualified name is valid
  check :: (Show a, Eq a) => String -> (Exports -> [a]) -> ModuleName -> a -> Either String (Qualified a)
  check t get mn' name = do
    modExports <- getExports mn'
    if name `elem` get modExports
      then return $ Qualified (Just mn') name
      else throwError $ "Unknown " ++ t ++ " '" ++ show (Qualified (Just mn') name) ++ "'"

  -- Gets the exports for a module, or an error message if the module doesn't exist
  getExports :: ModuleName -> Either String Exports
  getExports mn' = maybe (throwError $ "Unknown module '" ++ show mn' ++ "'") return $ M.lookup mn' exports

-- |
-- Finds all exported declarations in a set of modules.
--
findExports :: [Module] -> Either String ExportEnvironment
findExports = foldM addModule $ M.singleton (ModuleName [ProperName "Prim"]) primExports
  where

  -- The exported types from the Prim module
  primExports = Exports (mkTypeEntry `map` M.keys primTypes) [] []
    where
    mkTypeEntry (Qualified _ name) = (name, [])

  -- Add all of the exported declarations from a module to the global export environment
  addModule :: ExportEnvironment -> Module -> Either String ExportEnvironment
  addModule env m@(Module mn ds _) = rethrowForModule m $ foldM (addDecl mn) (addEmptyModule env mn) ds

  -- Add a declaration from a module to the global export environment
  addDecl :: ModuleName -> ExportEnvironment -> Declaration -> Either String ExportEnvironment
  addDecl mn env (TypeClassDeclaration tcn _ ds) = do
    env' <- addTypeClass env mn tcn
    foldM (\env'' (TypeDeclaration name _) -> addValue env'' mn name) env' ds
  addDecl mn env (DataDeclaration tn _ dcs) = addType env mn tn (map fst dcs)
  addDecl mn env (TypeSynonymDeclaration tn _ _) = addType env mn tn []
  addDecl mn env (ExternDataDeclaration tn _) = addType env mn tn []
  addDecl mn env (ValueDeclaration name _ _ _) = addValue env mn name
  addDecl mn env (ExternDeclaration _ name _ _) = addValue env mn name
  addDecl _  env _ = return env

-- |
-- Filters the exports for a module to ensure only explicit exports are kept in the global exports
-- environment.
--
filterExports :: ModuleName -> [DeclarationRef] -> ExportEnvironment -> Either String ExportEnvironment
filterExports mn exps env = do
  let moduleExports = fromMaybe (error "Module is missing") (mn `M.lookup` env)
  moduleExports' <- filterModule moduleExports
  return $ M.insert mn moduleExports' env
  where

  -- Filter the exports for the specific module
  filterModule :: Exports -> Either String Exports
  filterModule exported = do
    types <- foldM (filterTypes $ exportedTypes exported) [] exps
    values <- foldM (filterValues $ exportedValues exported) [] exps
    classes <- foldM (filterClasses $ exportedTypeClasses exported) [] exps
    return exported { exportedTypes = types, exportedTypeClasses = classes, exportedValues = values }

  -- Ensure the exported types and data constructors exist in the module and add them to the set of
  -- exports
  filterTypes :: [(ProperName, [ProperName])] -> [(ProperName, [ProperName])] -> DeclarationRef -> Either String [(ProperName, [ProperName])]
  filterTypes expTys result (TypeRef name expDcons) = do
    dcons <- maybe (throwError $ "Cannot export undefined type '" ++ show name ++ "'") return $ name `lookup` expTys
    dcons' <- maybe (return dcons) (foldM (filterDcons name dcons) []) expDcons
    return $ (name, dcons') : result
  filterTypes _ result _ = return result

  -- Ensure the exported data constructors exists for a type and add them to the list of exports
  filterDcons :: ProperName -> [ProperName] -> [ProperName] -> ProperName -> Either String [ProperName]
  filterDcons tcon exps' result name =
    if name `elem` exps'
    then return $ name : result
    else throwError $ "Cannot export undefined data constructor '" ++ show name ++ "' for type '" ++ show tcon ++ "'"

  -- Ensure the exported classes exist in the module and add them to the set of exports
  filterClasses :: [ProperName] -> [ProperName] -> DeclarationRef -> Either String [ProperName]
  filterClasses exps' result (TypeClassRef name) =
    if name `elem` exps'
    then return $ name : result
    else throwError $ "Cannot export undefined type class '" ++ show name ++ "'"
  filterClasses _ result _ = return result

  -- Ensure the exported values exist in the module and add them to the set of exports
  filterValues :: [Ident] -> [Ident] -> DeclarationRef -> Either String [Ident]
  filterValues exps' result (ValueRef name) =
    if name `elem` exps'
    then return $ name : result
    else throwError $ "Cannot export undefined value '" ++ show name ++ "'"
  filterValues _ result _ = return result

-- |
-- Type representing a set of declarations being explicitly imported from a module
--
type ExplicitImports = [DeclarationRef]

-- |
-- Finds the imports within a module, mapping the imported module name to an optional set of
-- explicitly imported declarations.
--
findImports :: [Declaration] -> M.Map ModuleName (Maybe ExplicitImports)
findImports = foldl findImports' M.empty
  where
  findImports' result (ImportDeclaration mn expl) = M.insert mn expl result
  findImports' result _ = result

-- |
-- Constructs a local environment for a module.
--
resolveImports :: ExportEnvironment -> Module -> Either String ImportEnvironment
resolveImports env (Module currentModule decls _) =
  foldM resolveImport' (ImportEnvironment M.empty M.empty M.empty M.empty) (M.toList scope)
  where
  -- A Map from module name to imports from that module, where Nothing indicates everything is to be imported
  scope :: M.Map ModuleName (Maybe ExplicitImports)
  scope = M.insert currentModule Nothing (findImports decls)
  resolveImport' imp (mn, i) = do
      m <- maybe (throwError $ "Cannot import unknown module '" ++ show mn ++ "'") return $ mn `M.lookup` env
      resolveImport currentModule mn m imp i

-- |
-- Extends the local environment for a module by resolving an import of another module.
--
resolveImport :: ModuleName -> ModuleName -> Exports -> ImportEnvironment -> Maybe ExplicitImports -> Either String ImportEnvironment
resolveImport currentModule importModule exps imps = maybe importAll (foldM importExplicit imps)
  where

  -- Import everything from a module
  importAll :: Either String ImportEnvironment
  importAll = do
    imp' <- foldM (\m (name, dctors) -> importExplicit m (TypeRef name (Just dctors))) imps (exportedTypes exps)
    imp'' <- foldM (\m name -> importExplicit m (ValueRef name)) imp' (exportedValues exps)
    foldM (\m name -> importExplicit m (TypeClassRef name)) imp'' (exportedTypeClasses exps)

  -- Import something explicitly
  importExplicit :: ImportEnvironment -> DeclarationRef -> Either String ImportEnvironment
  importExplicit imp (ValueRef name) = do
    _ <- checkImportExists "value" values name
    values' <- updateImports (importedValues imp) name
    return $ imp { importedValues = values' }
  importExplicit imp (TypeRef name dctors) = do
    _ <- checkImportExists "type" types name
    types' <- updateImports (importedTypes imp) name
    let allDctors = allExportedDataConstructors name
    dctors' <- maybe (return allDctors) (mapM $ checkDctorExists allDctors) dctors
    dctors'' <- foldM updateImports (importedDataConstructors imp) dctors'
    return $ imp { importedTypes = types', importedDataConstructors = dctors'' }
  importExplicit imp (TypeClassRef name) = do
    _ <- checkImportExists "type class" classes name
    typeClasses' <- updateImports (importedTypeClasses imp) name
    return $ imp { importedTypeClasses = typeClasses' }

  -- Find all exported data constructors for a given type
  allExportedDataConstructors :: ProperName -> [ProperName]
  allExportedDataConstructors name = fromMaybe [] $ name `lookup` exportedTypes exps

  -- Add something to the ImportEnvironment if it does not already exist there
  updateImports :: (Ord a, Show a) => M.Map a (Qualified a) -> a -> Either String (M.Map a (Qualified a))
  updateImports m name = case M.lookup name m of
    Nothing -> return $ M.insert name (Qualified (Just importModule) name) m
    Just (Qualified Nothing _) -> error "Invalid state in updateImports"
    Just x@(Qualified (Just mn) _) -> throwError $
      if mn == currentModule || importModule == currentModule
      then "Definition '" ++ show name ++ "' conflicts with import '" ++ show (Qualified (Just importModule) name) ++ "'"
      else "Conflicting imports for '" ++ show name ++ "': '" ++ show x ++ "', '" ++ show (Qualified (Just importModule) name) ++ "'"

  -- The available values, types, and classes in the module being imported
  values = exportedValues exps
  types = fst `map` exportedTypes exps
  classes = exportedTypeClasses exps

  -- Ensure that an explicitly imported data constructor exists for the type it is being imported
  -- from
  checkDctorExists :: [ProperName] -> ProperName -> Either String ProperName
  checkDctorExists = checkImportExists "data constructor"

  -- Check that an explicitly imported item exists in the module it is being imported from
  checkImportExists :: (Eq a, Show a) => String -> [a] -> a -> Either String a
  checkImportExists t exports item =
      if item `elem` exports
      then return item
      else throwError $ "Unable to find " ++ t ++  " '" ++ show (Qualified (Just importModule) item) ++ "'"

