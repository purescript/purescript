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

import Data.Maybe (fromMaybe, isJust)
import Data.List (intersect, intercalate, (\\))
import Data.Data
import Data.Generics (extM, mkM, everywhereM)
import Data.Generics.Extras (mkS, extS, everywhereWithContextM')

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Monad (forM_, unless, foldM)
import Control.Monad.Error

import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Values

-- |
-- The global export environment - every declaration exported from every module
--
data ExportEnvironment = ExportEnvironment
    -- |
    -- The types exported from each module
    --
    { exportedTypes :: M.Map ModuleName (S.Set (ProperName, [ProperName]))
    -- |
    -- The classes exported from each module
    --
    , exportedTypeClasses :: M.Map ModuleName (S.Set ProperName)
    -- |
    -- The values exported from each module
    , exportedValues :: M.Map ModuleName (S.Set Ident)
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
-- Adds a type belonging to a module to the export environment.
--
addType :: ExportEnvironment -> ModuleName -> ProperName -> [ProperName] -> Either String ExportEnvironment
addType env mn name dctors = do
    types <- addExport (exportedTypes env) mn (name, dctors)
    return $ env { exportedTypes = types }

-- |
-- Adds a class to the export environment.
--
addTypeClass :: ExportEnvironment -> ModuleName -> ProperName -> Either String ExportEnvironment
addTypeClass env mn name = do
    classes <- addExport (exportedTypeClasses env) mn name
    return $ env { exportedTypeClasses = classes }

-- |
-- Adds a class to the export environment.
--
addValue :: ExportEnvironment -> ModuleName -> Ident -> Either String ExportEnvironment
addValue env mn name = do
    values <- addExport (exportedValues env) mn name
    return $ env { exportedValues = values }

-- |
-- Adds an export to a map of exports of that type.
--
addExport :: (Ord s, Show s) => M.Map ModuleName (S.Set s) -> ModuleName -> s -> Either String (M.Map ModuleName (S.Set s))
addExport exports mn name = case M.lookup mn exports of
    Just s -> if S.member name s
              then throwError $ "Multiple definitions for '" ++ show name ++ "'"
              else return $ M.insert mn (S.insert name s) exports
    Nothing -> return $ M.insert mn (S.singleton name) exports

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
    Module mn <$> mapM updateDecl decls >>= everywhereM (mkM updateType `extM` updateValue `extM` updateBinder)
    where
    updateDecl (TypeInstanceDeclaration cs (Qualified Nothing cn) ts ds) =
        TypeInstanceDeclaration <$> updateConstraints cs <*> updateClassName cn <*> pure ts <*> pure ds
    updateDecl (ValueDeclaration name bs grd val) =
        ValueDeclaration name <$> updateVars bs <*> updateVars grd <*> updateVars val
        where
        updateVars :: (Data d) => d -> Either String d
        updateVars = everywhereWithContextM' [] (mkS bindFunctionArgs `extS` bindBinders)
          where
          bindFunctionArgs bound (Abs (Left arg) val) = return (arg : bound, Abs (Left arg) val)
          bindFunctionArgs bound (Var (Qualified Nothing ident)) | ident `notElem` bound = (,) bound <$> (Var <$> updateValueName ident)
          bindFunctionArgs bound other = return (bound, other)
          bindBinders :: [Ident] -> ([Binder], Maybe Guard, Value) -> Either String ([Ident], ([Binder], Maybe Guard, Value))
          bindBinders bound (bs, grd, val) = return (binderNames bs ++ bound, (bs, grd, val))
    updateDecl d = return d
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
findExports = foldM addModule (ExportEnvironment M.empty M.empty M.empty)
    where
    addModule env m@(Module mn ds) = rethrowForModule m $ foldM (addDecl mn) env ds
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
    foldM resolveImport (ImportEnvironment M.empty M.empty M.empty M.empty) (M.toList scope)
    where
    -- A Map from module name to imports from that module, where Nothing indicates everything is to be imported
    scope :: M.Map ModuleName (Maybe ExplicitImports)
    scope = M.insert currentModule Nothing (findImports decls)

    -- Add data to the ImportEnvironment
    resolveImport :: ImportEnvironment -> (ModuleName, Maybe ExplicitImports) -> Either String ImportEnvironment
    resolveImport imp (mn, Nothing) = importAll imp mn env
    resolveImport imp (mn, Just expl) = foldM (importExplicit mn) imp expl

    -- Import something explicitly
    importExplicit :: ModuleName -> ImportEnvironment -> ImportType -> Either String ImportEnvironment
    importExplicit mn imp (NameImport name) = do
      checkValueExists env mn name
      values' <- updateImports mn (importedValues imp) name
      return $ imp { importedValues = values' }
    importExplicit mn imp (TypeImport name dctors) = do
      checkTypeExists env mn name
      types' <- updateImports mn (importedTypes imp) name
      let allDctors = allExportedDataConstructors env mn name
      dctors' <- maybe (return allDctors) (mapM $ checkDctorExists mn allDctors) dctors
      dctors'' <- foldM (updateImports mn) (importedDataConstructors imp) dctors'
      return $ imp { importedTypes = types', importedDataConstructors = dctors'' }
    importExplicit mn imp (TypeClassImport name) = do
      checkClassExists env mn name
      typeClasses' <- updateImports mn (importedTypeClasses imp) name
      return $ imp { importedTypeClasses = typeClasses' }

    -- Import everything from a module
    importAll :: ImportEnvironment -> ModuleName -> ExportEnvironment -> Either String ImportEnvironment
    importAll imp mn env = do
      imp' <- foldM (\m (name, dctors) -> importExplicit mn m (TypeImport name (Just dctors))) imp
        (S.toList . fromMaybe S.empty $ mn `M.lookup` exportedTypes env)
      imp'' <- foldM (\m name -> importExplicit mn m (NameImport name)) imp'
        (S.toList . fromMaybe S.empty $ mn `M.lookup` exportedValues env)
      foldM (\m name -> importExplicit mn m (TypeClassImport name)) imp''
        (S.toList . fromMaybe S.empty $ mn `M.lookup` exportedTypeClasses env)

    -- Find all exported data constructors for a given type
    allExportedDataConstructors :: ExportEnvironment -> ModuleName -> ProperName -> [ProperName]
    allExportedDataConstructors env mn name = fromMaybe [] $ do
      s <- mn `M.lookup` exportedTypes env
      name `lookup` S.toList s

    -- Add something to the ImportEnvironment if it does not already exist there
    updateImports :: (Ord id, Show id) => ModuleName -> M.Map id (Qualified id) -> id -> Either String (M.Map id (Qualified id))
    updateImports mn m name = case M.lookup name m of
      Nothing -> return $ M.insert name (Qualified (Just mn) name) m
      Just x@(Qualified (Just mn') _) -> throwError $
        if mn' == currentModule || mn == currentModule
        then "Definition '" ++ show name ++ "' conflicts with import '" ++ show (Qualified (Just mn) name) ++ "'"
        else "Conflicting imports for '" ++ show name ++ "': '" ++ show x ++ "', '" ++ show (Qualified (Just mn) name) ++ "'"

    -- Ensure that an explicitly imported value exists in the module it is being imported from
    checkValueExists :: ExportEnvironment -> ModuleName -> Ident -> Either String Ident
    checkValueExists env mn = checkImportExists "value" mn values
        where values = S.toList $ fromMaybe S.empty $ mn `M.lookup` exportedValues env

    -- Ensure that an explicitly imported type exists in the module it is being imported from
    checkTypeExists :: ExportEnvironment -> ModuleName -> ProperName -> Either String ProperName
    checkTypeExists env mn = checkImportExists "type" mn types
        where types = fst `map` S.toList (fromMaybe S.empty $ mn `M.lookup` exportedTypes env)

    -- Ensure that an explicitly imported class exists in the module it is being imported from
    checkClassExists :: ExportEnvironment -> ModuleName -> ProperName -> Either String ProperName
    checkClassExists env mn = checkImportExists "type class" mn classes
        where classes = S.toList $ fromMaybe S.empty $ mn `M.lookup` exportedTypeClasses env

    -- Ensure that an explicitly imported data constructor exists for the type it is being imported
    -- from
    checkDctorExists :: ModuleName -> [ProperName] -> ProperName -> Either String ProperName
    checkDctorExists = checkImportExists "data constructor"

    -- Check that an explicitly imported item exists in the module it is being imported from
    checkImportExists :: (Show a, Eq a) => String -> ModuleName -> [a] -> a -> Either String a
    checkImportExists t mn exports item =
        if item `elem` exports
        then return item
        else throwError $ "Unable to find " ++ t ++  " '" ++ show (Qualified (Just mn) item) ++ "'"

