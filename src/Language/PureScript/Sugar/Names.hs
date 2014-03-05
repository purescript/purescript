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
import Data.Data
import Data.Generics (extM, mkM, everywhereM)
import Data.Generics.Extras (mkS, extS, everywhereWithContextM')

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Monad (foldM)
import Control.Monad.Error

import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Values

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
    { exportedTypes :: S.Set (ProperName, [ProperName])
    -- |
    -- The classes exported from each module
    --
    , exportedTypeClasses :: S.Set ProperName
    -- |
    -- The values exported from each module
    , exportedValues :: S.Set Ident
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
addEmptyModule env name = M.insert name (Exports S.empty S.empty S.empty) env

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
-- Adds an export to a map of exports of that type.
--
addExport :: (Ord s, Show s) => S.Set s -> s -> Either String (S.Set s)
addExport exports name =
    if S.member name exports
    then throwError $ "Multiple definitions for '" ++ show name ++ "'"
    else return $ S.insert name exports

-- |
-- Replaces all local names with qualified names within a set of modules.
--
desugarImports :: [Module] -> Either String [Module]
desugarImports modules = do
    exports <- findExports modules
    mapM (renameInModule' exports) modules
    where
    renameInModule' exports m = rethrowForModule m $ do
        imports <- resolveImports exports m
        renameInModule imports m

-- |
-- Rethrow an error with the name of the current module in the case of a failure
--
rethrowForModule :: Module -> Either String a -> Either String a
rethrowForModule (Module mn _) = flip catchError $ \e -> throwError ("Error in module '" ++ show mn ++ "':\n" ++  e)

-- |
-- Replaces all local names with qualified names within a module.
--
renameInModule :: ImportEnvironment -> Module -> Either String Module
renameInModule imports (Module mn decls) =
    Module mn <$> mapM updateDecl decls >>= everywhereM (mkM updateType `extM` updateValue `extM` updateBinder `extM` updateVars)
    where
    updateDecl (TypeInstanceDeclaration name cs (Qualified Nothing cn) ts ds) =
        TypeInstanceDeclaration name <$> updateConstraints cs <*> updateClassName cn <*> pure ts <*> pure ds
    updateDecl d = return d

    updateVars :: Declaration -> Either String Declaration
    updateVars (ValueDeclaration name [] Nothing val) =
      ValueDeclaration name [] Nothing <$> everywhereWithContextM' [] (mkS bindFunctionArgs `extS` bindBinders) val
      where
      bindFunctionArgs bound (Abs (Left arg) val) = return (arg : bound, Abs (Left arg) val)
      bindFunctionArgs bound (Var (Qualified Nothing ident)) | ident `notElem` bound = (,) bound <$> (Var <$> updateValueName ident)
      bindFunctionArgs bound other = return (bound, other)
      bindBinders :: [Ident] -> CaseAlternative -> Either String ([Ident], CaseAlternative)
      bindBinders bound c@(CaseAlternative bs _ _) = return (binderNames bs ++ bound, c)
    updateVars (ValueDeclaration name _ _ _) = error $ "Binders should have been desugared in " ++ show name
    updateVars other = return other

    updateValue (Constructor (Qualified Nothing nm)) =
                 Constructor <$> updateDataConstructorName nm
    updateValue v = return v

    updateBinder (ConstructorBinder (Qualified Nothing nm) b) =
                  ConstructorBinder <$> updateDataConstructorName nm <*> pure b
    updateBinder v = return v
    updateType (TypeConstructor (Qualified Nothing nm)) =
                TypeConstructor <$> updateTypeName nm
    updateType (SaturatedTypeSynonym (Qualified Nothing nm) tys) =
                SaturatedTypeSynonym <$> updateTypeName nm <*> mapM updateType tys
    updateType (ConstrainedType cs t) =
                ConstrainedType <$> updateConstraints cs <*> pure t
    updateType t = return t
    updateConstraints = mapM updateConstraint
    updateConstraint (Qualified Nothing nm, ts) = (,) <$> updateClassName nm <*> pure ts
    updateConstraint other = return other
    updateTypeName = update "type" importedTypes
    updateClassName = update "type class" importedTypeClasses
    updateValueName = update "value" importedValues
    updateDataConstructorName = update "data constructor" importedDataConstructors
    update t get nm = maybe (throwError $ "Unknown " ++ t ++ " '" ++ show nm ++ "'") return $ M.lookup nm (get imports)

-- |
-- Finds all exported declarations in a set of modules.
--
findExports :: [Module] -> Either String ExportEnvironment
findExports = foldM addModule M.empty
    where
    addModule env m@(Module mn ds) = rethrowForModule m $ foldM (addDecl mn) (addEmptyModule env mn) ds
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
-- Type representing a set of declarations being explicitly imported from a module
--
type ExplicitImports = [ImportType]

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
resolveImports env (Module currentModule decls) =
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
resolveImport currentModule importModule exp imp i = case i of
    Nothing -> importAll imp
    (Just expl) -> foldM importExplicit imp expl
    where

    -- Import everything from a module
    importAll :: ImportEnvironment -> Either String ImportEnvironment
    importAll imp = do
      imp' <- foldM (\m (name, dctors) -> importExplicit m (TypeImport name (Just dctors))) imp (S.toList $ exportedTypes exp)
      imp'' <- foldM (\m name -> importExplicit m (NameImport name)) imp' (S.toList $ exportedValues exp)
      foldM (\m name -> importExplicit m (TypeClassImport name)) imp'' (S.toList $ exportedTypeClasses exp)

    -- Import something explicitly
    importExplicit :: ImportEnvironment -> ImportType -> Either String ImportEnvironment
    importExplicit imp (NameImport name) = do
      checkImportExists "value" values name
      values' <- updateImports (importedValues imp) name
      return $ imp { importedValues = values' }
    importExplicit imp (TypeImport name dctors) = do
      checkImportExists "type" types name
      types' <- updateImports (importedTypes imp) name
      let allDctors = allExportedDataConstructors name
      dctors' <- maybe (return allDctors) (mapM $ checkDctorExists allDctors) dctors
      dctors'' <- foldM updateImports (importedDataConstructors imp) dctors'
      return $ imp { importedTypes = types', importedDataConstructors = dctors'' }
    importExplicit imp (TypeClassImport name) = do
      checkImportExists "type class" classes name
      typeClasses' <- updateImports (importedTypeClasses imp) name
      return $ imp { importedTypeClasses = typeClasses' }

    -- Find all exported data constructors for a given type
    allExportedDataConstructors :: ProperName -> [ProperName]
    allExportedDataConstructors name = fromMaybe [] $ name `lookup` S.toList (exportedTypes exp)

    -- Add something to the ImportEnvironment if it does not already exist there
    updateImports :: (Ord id, Show id) => M.Map id (Qualified id) -> id -> Either String (M.Map id (Qualified id))
    updateImports m name = case M.lookup name m of
      Nothing -> return $ M.insert name (Qualified (Just importModule) name) m
      Just x@(Qualified (Just mn) _) -> throwError $
        if mn == currentModule || importModule == currentModule
        then "Definition '" ++ show name ++ "' conflicts with import '" ++ show (Qualified (Just importModule) name) ++ "'"
        else "Conflicting imports for '" ++ show name ++ "': '" ++ show x ++ "', '" ++ show (Qualified (Just importModule) name) ++ "'"

    -- The available values, types, and classes in the module being imported
    values = exportedValues exp
    types = fst `S.map` exportedTypes exp
    classes = exportedTypeClasses exp

    -- Ensure that an explicitly imported data constructor exists for the type it is being imported
    -- from
    checkDctorExists :: [ProperName] -> ProperName -> Either String ProperName
    checkDctorExists names = checkImportExists "data constructor" (S.fromList names)

    -- Check that an explicitly imported item exists in the module it is being imported from
    checkImportExists :: (Show a, Ord a, Eq a) => String -> S.Set a -> a -> Either String a
    checkImportExists t exports item =
        if item `S.member` exports
        then return item
        else throwError $ "Unable to find " ++ t ++  " '" ++ show (Qualified (Just importModule) item) ++ "'"

