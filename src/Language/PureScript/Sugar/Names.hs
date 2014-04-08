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

import Data.Data
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Monoid ((<>))
import Data.Generics.Extras (mkS, extS, everywhereWithContextM')

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Monad.Error

import qualified Data.Map as M

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Environment
import Language.PureScript.Errors

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
  { importedTypes :: M.Map (Qualified ProperName) (Qualified ProperName)
  -- |
  -- Local names for data constructors within a module mapped to to their qualified names
  --
  , importedDataConstructors :: M.Map (Qualified ProperName) (Qualified ProperName)
  -- |
  -- Local names for classes within a module mapped to to their qualified names
  --
  , importedTypeClasses :: M.Map (Qualified ProperName) (Qualified ProperName)
  -- |
  -- Local names for values within a module mapped to to their qualified names
  --
  , importedValues :: M.Map (Qualified Ident) (Qualified Ident)
  } deriving (Show)

-- |
-- Updates the exports for a module from the global environment. If the module was not previously
-- present in the global environment, it is created.
--
updateExportedModule :: ExportEnvironment -> ModuleName -> (Exports -> Either ErrorStack Exports) -> Either ErrorStack ExportEnvironment
updateExportedModule env mn update = do
  let exports = fromMaybe (error "Module was undefined in updateExportedModule") $ mn `M.lookup` env
  exports' <- update exports
  return $ M.insert mn exports' env

-- |
-- Adds an empty module to an ExportEnvironment.
--
addEmptyModule :: ExportEnvironment -> ModuleName -> Either ErrorStack ExportEnvironment
addEmptyModule env name =
  if name `M.member` env
    then throwError $ mkErrorStack ("Module '" ++ show name ++ "' has been defined more than once") Nothing
    else return $ M.insert name (Exports [] [] []) env

-- |
-- Adds a type belonging to a module to the export environment.
--
addType :: ExportEnvironment -> ModuleName -> ProperName -> [ProperName] -> Either ErrorStack ExportEnvironment
addType env mn name dctors = updateExportedModule env mn $ \m -> do
  types' <- addExport (exportedTypes m) (name, dctors)
  return $ m { exportedTypes = types' }

-- |
-- Adds a class to the export environment.
--
addTypeClass :: ExportEnvironment -> ModuleName -> ProperName -> Either ErrorStack ExportEnvironment
addTypeClass env mn name = updateExportedModule env mn $ \m -> do
  classes <- addExport (exportedTypeClasses m) name
  return $ m { exportedTypeClasses = classes }

-- |
-- Adds a class to the export environment.
--
addValue :: ExportEnvironment -> ModuleName -> Ident -> Either ErrorStack ExportEnvironment
addValue env mn name = updateExportedModule env mn $ \m -> do
  values <- addExport (exportedValues m) name
  return $ m { exportedValues = values }

-- |
-- Adds an entry to a list of exports unless it is already present, in which case an error is
-- returned.
--
addExport :: (Eq a, Show a) => [a] -> a -> Either ErrorStack [a]
addExport exports name =
  if name `elem` exports
  then throwError $ mkErrorStack ("Multiple definitions for '" ++ show name ++ "'") Nothing
  else return $ name : exports

-- |
-- Replaces all local names with qualified names within a set of modules.
--
desugarImports :: [Module] -> Either ErrorStack [Module]
desugarImports modules = do
  unfilteredExports <- findExports modules
  exports <- foldM filterModuleExports unfilteredExports modules
  mapM (renameInModule' unfilteredExports exports) modules
  where

  -- Filters the exports for a module in the global exports environment so that only explicitly
  -- exported members remain. If the module does not explicitly export anything, everything is
  -- exported.
  filterModuleExports :: ExportEnvironment -> Module -> Either ErrorStack ExportEnvironment
  filterModuleExports env (Module mn _ (Just exps)) = filterExports mn exps env
  filterModuleExports env _ = return env

  -- Rename and check all the names within a module. We tweak the global exports environment so
  -- the module has access to an unfiltered list of its own members.
  renameInModule' :: ExportEnvironment -> ExportEnvironment -> Module -> Either ErrorStack Module
  renameInModule' unfilteredExports exports m@(Module mn _ _) =
    rethrow (strMsg ("Error in module " ++ show mn) <>) $ do
      let env = M.update (\_ -> M.lookup mn unfilteredExports) mn exports
      let exps = fromMaybe (error "Module is missing in renameInModule'") $ M.lookup mn exports
      imports <- resolveImports env m
      renameInModule imports env (elaborateExports exps m)

-- |
-- Make all exports for a module explicit. This may still effect modules that have an exports list,
-- as it will also make all data constructor exports explicit.
--
elaborateExports :: Exports -> Module -> Module
elaborateExports exps (Module mn decls _) = Module mn decls (Just $
  map (\(ctor, dctors) -> TypeRef ctor (Just dctors)) (exportedTypes exps) ++
  map TypeClassRef (exportedTypeClasses exps) ++
  map ValueRef (exportedValues exps))

-- |
-- Replaces all local names with qualified names within a module and checks that all existing
-- qualified names are valid.
--
renameInModule :: ImportEnvironment -> ExportEnvironment -> Module -> Either ErrorStack Module
renameInModule imports exports (Module mn decls exps) =
  Module mn <$> mapM go decls <*> pure exps
  where
  go :: Declaration -> Either ErrorStack Declaration
  go (PositionedDeclaration pos d) = rethrowWithPosition pos $ PositionedDeclaration pos <$> go d
  go (DataDeclaration name args dctors) =
      rethrow (strMsg ("Error in data declaration " ++ show name) <>) $
        DataDeclaration <$> pure name <*> pure args <*> updateAll dctors
  go (DataBindingGroupDeclaration decls') =
      DataBindingGroupDeclaration <$> mapM go decls'
  go (TypeSynonymDeclaration name ps ty) =
      rethrow (strMsg ("Error in type synonym " ++ show name) <>) $
        TypeSynonymDeclaration <$> pure name <*> pure ps <*> updateType' ty
  go (TypeClassDeclaration className args implies ds) =
    TypeClassDeclaration className args <$> updateConstraints Nothing implies <*> mapM go ds
  go (TypeInstanceDeclaration name cs cn ts ds) =
      TypeInstanceDeclaration name <$> updateConstraints Nothing cs <*> updateClassName cn Nothing <*> updateType' ts <*> mapM go ds
  go (ExternInstanceDeclaration name cs cn ts) =
      ExternInstanceDeclaration name <$> updateConstraints Nothing cs <*> updateClassName cn Nothing <*> updateType' ts
  go (ValueDeclaration name nameKind [] Nothing val) = do
    val' <- everywhereWithContextM' (Nothing, []) (mkS bindFunctionArgs `extS` bindBinders) val
    rethrow (strMsg ("Error in declaration " ++ show name) <>) $
      ValueDeclaration name nameKind [] Nothing <$> updateAll val'
    where
    bindFunctionArgs :: (Maybe SourcePos, [Ident]) -> Value -> Either ErrorStack ((Maybe SourcePos, [Ident]), Value)
    bindFunctionArgs (_, bound) v@(PositionedValue pos' _) = return ((Just pos', bound), v)
    bindFunctionArgs (pos, bound) (Abs (Left arg) val') = return ((pos, arg : bound), Abs (Left arg) val')
    bindFunctionArgs (pos, bound) (Let ds val') =
      let args = mapMaybe letBoundVariable ds
      in return ((pos, args ++ bound), Let ds val')
    bindFunctionArgs (pos, bound) (Var name'@(Qualified Nothing ident)) | ident `notElem` bound =
      (,) (pos, bound) <$> (Var <$> updateValueName name' pos)
    bindFunctionArgs (pos, bound) (Var name'@(Qualified (Just _) _)) =
      (,) (pos, bound) <$> (Var <$> updateValueName name' pos)
    bindFunctionArgs (pos, bound) (BinaryNoParens name'@(Qualified Nothing ident) v1 v2) | ident `notElem` bound =
      (,) (pos, bound) <$> (BinaryNoParens <$> updateValueName name' pos <*> pure v1 <*> pure v2)
    bindFunctionArgs (pos, bound) (BinaryNoParens name'@(Qualified (Just _) _) v1 v2) =
      (,) (pos, bound) <$> (BinaryNoParens <$> updateValueName name' pos <*> pure v1 <*> pure v2)
    bindFunctionArgs pb other = return (pb, other)

    bindBinders :: (Maybe SourcePos, [Ident]) -> CaseAlternative -> Either ErrorStack ((Maybe SourcePos, [Ident]), CaseAlternative)
    bindBinders (pos, bound) c@(CaseAlternative bs _ _) = return ((pos, binderNames bs ++ bound), c)

    letBoundVariable :: Declaration -> Maybe Ident
    letBoundVariable (ValueDeclaration ident _ _ _ _) = Just ident
    letBoundVariable (PositionedDeclaration _ d) = letBoundVariable d
    letBoundVariable _ = Nothing
  go (ValueDeclaration name _ _ _ _) = error $ "Binders should have been desugared in " ++ show name
  go (ExternDeclaration fit name js ty) =
      rethrow (strMsg ("Error in declaration " ++ show name) <>) $
        ExternDeclaration <$> pure fit <*> pure name <*> pure js <*> updateType' ty
  go (BindingGroupDeclaration decls') = BindingGroupDeclaration <$> mapM go' decls'
      where
      go' (name, nk, value) = rethrow (strMsg ("Error in declaration " ++ show name) <>) $
                                (,,) <$> pure name <*> pure nk <*> updateAll value
  go d = updateAll d

  updateAll :: Data d => d -> Either ErrorStack d
  updateAll = everywhereWithContextM' Nothing (mkS updateType `extS` updateValue `extS` updateBinder)

  updateValue :: Maybe SourcePos -> Value -> Either ErrorStack (Maybe SourcePos, Value)
  updateValue _ v@(PositionedValue pos _) = return (Just pos, v)
  updateValue pos (Constructor name) = (,) <$> pure pos <*> (Constructor <$> updateDataConstructorName name pos)
  updateValue pos v = return (pos, v)

  updateBinder :: Maybe SourcePos -> Binder -> Either ErrorStack (Maybe SourcePos, Binder)
  updateBinder _ v@(PositionedBinder pos _) = return (Just pos, v)
  updateBinder pos (ConstructorBinder name b) = (,) <$> pure pos <*> (ConstructorBinder <$> updateDataConstructorName name pos <*> pure b)
  updateBinder pos v = return (pos, v)

  updateType :: Maybe SourcePos -> Type -> Either ErrorStack (Maybe SourcePos, Type)
  updateType pos (TypeConstructor name) = (,) <$> pure pos <*> (TypeConstructor <$> updateTypeName name pos)
  updateType pos (SaturatedTypeSynonym name tys) = (,) <$> pure pos <*> (SaturatedTypeSynonym <$> updateTypeName name pos <*> updateType' tys)
  updateType pos (ConstrainedType cs t) = (,) <$> pure pos <*> (ConstrainedType <$> updateConstraints pos cs <*> pure t)
  updateType pos t = return (pos, t)

  updateType' :: Data d => d -> Either ErrorStack d
  updateType' = everywhereWithContextM' Nothing (mkS updateType)

  updateConstraints pos = mapM (\(name, ts) -> (,) <$> updateClassName name pos <*> pure ts)

  updateTypeName = update "type" importedTypes (\mes -> isJust . (`lookup` exportedTypes mes))
  updateClassName = update "type class" importedTypeClasses (flip elem . exportedTypeClasses)
  updateValueName = update "value" importedValues (flip elem . exportedValues)
  updateDataConstructorName = update "data constructor" importedDataConstructors (\mes -> flip elem (join $ snd `map` exportedTypes mes))

  -- Update names so unqualified references become qualified, and locally qualified references
  -- are replaced with their canoncial qualified names (e.g. M.Map -> Data.Map.Map)
  update :: (Ord a, Show a) => String
                            -> (ImportEnvironment -> M.Map (Qualified a) (Qualified a))
                            -> (Exports -> a -> Bool)
                            -> Qualified a
                            -> Maybe SourcePos
                            -> Either ErrorStack (Qualified a)
  update t getI checkE qname@(Qualified mn' name) pos = case (M.lookup qname (getI imports), mn') of
    (Just qname', _) -> return qname'
    (Nothing, Just mn'') -> do
      modExports <- getExports mn''
      if checkE modExports name
        then return qname
        else positioned $ throwError $ mkErrorStack ("Unknown " ++ t ++ " '" ++ show qname ++ "'") Nothing
    _ -> positioned $ throwError $ mkErrorStack ("Unknown " ++ t ++ " '" ++ show name ++ "'") Nothing
    where
    positioned err = case pos of
      Nothing -> err
      Just pos' -> rethrowWithPosition pos' err

  -- Gets the exports for a module, or an error message if the module doesn't exist
  getExports :: ModuleName -> Either ErrorStack Exports
  getExports mn' = maybe (throwError $ mkErrorStack ("Unknown module '" ++ show mn' ++ "'") Nothing) return $ M.lookup mn' exports

-- |
-- Finds all exported declarations in a set of modules.
--
findExports :: [Module] -> Either ErrorStack ExportEnvironment
findExports = foldM addModule $ M.singleton (ModuleName [ProperName "Prim"]) primExports
  where

  -- The exported types from the Prim module
  primExports = Exports (mkTypeEntry `map` M.keys primTypes) [] []
    where
    mkTypeEntry (Qualified _ name) = (name, [])

  -- Add all of the exported declarations from a module to the global export environment
  addModule :: ExportEnvironment -> Module -> Either ErrorStack ExportEnvironment
  addModule env (Module mn ds _) = do
    env' <- addEmptyModule env mn
    rethrow (strMsg ("Error in module " ++ show mn) <>) $ foldM (addDecl mn) env' ds

  -- Add a declaration from a module to the global export environment
  addDecl :: ModuleName -> ExportEnvironment -> Declaration -> Either ErrorStack ExportEnvironment
  addDecl mn env (TypeClassDeclaration tcn _ _ ds) = do
    env' <- addTypeClass env mn tcn
    foldM go env' ds
    where
    go env'' (TypeDeclaration name _) = addValue env'' mn name
    go env'' (PositionedDeclaration pos d) = rethrowWithPosition pos $ go env'' d
    go _ _ = error "Invalid declaration in TypeClassDeclaration"
  addDecl mn env (DataDeclaration tn _ dcs) = addType env mn tn (map fst dcs)
  addDecl mn env (TypeSynonymDeclaration tn _ _) = addType env mn tn []
  addDecl mn env (ExternDataDeclaration tn _) = addType env mn tn []
  addDecl mn env (ValueDeclaration name _ _ _ _) = addValue env mn name
  addDecl mn env (ExternDeclaration _ name _ _) = addValue env mn name
  addDecl mn env (PositionedDeclaration _ d) = addDecl mn env d
  addDecl _  env _ = return env

-- |
-- Filters the exports for a module to ensure only explicit exports are kept in the global exports
-- environment.
--
filterExports :: ModuleName -> [DeclarationRef] -> ExportEnvironment -> Either ErrorStack ExportEnvironment
filterExports mn exps env = do
  let moduleExports = fromMaybe (error "Module is missing") (mn `M.lookup` env)
  moduleExports' <- rethrow (strMsg ("Error in module " ++ show mn) <>) $ filterModule moduleExports
  return $ M.insert mn moduleExports' env
  where

  -- Filter the exports for the specific module
  filterModule :: Exports -> Either ErrorStack Exports
  filterModule exported = do
    types' <- foldM (filterTypes $ exportedTypes exported) [] exps
    values <- foldM (filterValues $ exportedValues exported) [] exps
    classes <- foldM (filterClasses $ exportedTypeClasses exported) [] exps
    return exported { exportedTypes = types', exportedTypeClasses = classes, exportedValues = values }

  -- Ensure the exported types and data constructors exist in the module and add them to the set of
  -- exports
  filterTypes :: [(ProperName, [ProperName])] -> [(ProperName, [ProperName])] -> DeclarationRef -> Either ErrorStack [(ProperName, [ProperName])]
  filterTypes expTys result (PositionedDeclarationRef pos r) = rethrowWithPosition pos $ filterTypes expTys result r
  filterTypes expTys result (TypeRef name expDcons) = do
    dcons <- maybe (throwError $ mkErrorStack ("Cannot export undefined type '" ++ show name ++ "'") Nothing) return $ name `lookup` expTys
    dcons' <- maybe (return dcons) (foldM (filterDcons name dcons) []) expDcons
    return $ (name, dcons') : result
  filterTypes _ result _ = return result

  -- Ensure the exported data constructors exists for a type and add them to the list of exports
  filterDcons :: ProperName -> [ProperName] -> [ProperName] -> ProperName -> Either ErrorStack [ProperName]
  filterDcons tcon exps' result name =
    if name `elem` exps'
    then return $ name : result
    else throwError $ mkErrorStack ("Cannot export undefined data constructor '" ++ show name ++ "' for type '" ++ show tcon ++ "'") Nothing

  -- Ensure the exported classes exist in the module and add them to the set of exports
  filterClasses :: [ProperName] -> [ProperName] -> DeclarationRef -> Either ErrorStack [ProperName]
  filterClasses exps' result (PositionedDeclarationRef pos r) = rethrowWithPosition pos $ filterClasses exps' result r
  filterClasses exps' result (TypeClassRef name) =
    if name `elem` exps'
    then return $ name : result
    else throwError $ mkErrorStack ("Cannot export undefined type class '" ++ show name ++ "'") Nothing
  filterClasses _ result _ = return result

  -- Ensure the exported values exist in the module and add them to the set of exports
  filterValues :: [Ident] -> [Ident] -> DeclarationRef -> Either ErrorStack [Ident]
  filterValues exps' result (PositionedDeclarationRef pos r) = rethrowWithPosition pos $ filterValues exps' result r
  filterValues exps' result (ValueRef name) =
    if name `elem` exps'
    then return $ name : result
    else throwError $ mkErrorStack ("Cannot export undefined value '" ++ show name ++ "'") Nothing
  filterValues _ result _ = return result

-- |
-- Type representing a set of declarations being explicitly imported from a module
--
type ExplicitImports = [DeclarationRef]

-- |
-- Finds the imports within a module, mapping the imported module name to an optional set of
-- explicitly imported declarations.
--
findImports :: [Declaration] -> M.Map ModuleName (Maybe SourcePos, Maybe ExplicitImports, Maybe ModuleName)
findImports = foldl (findImports' Nothing) M.empty
  where
  findImports' pos result (ImportDeclaration mn expl qual) = M.insert mn (pos, expl, qual) result
  findImports' _ result (PositionedDeclaration pos d) = findImports' (Just pos) result d
  findImports' _ result _ = result

-- |
-- Constructs a local environment for a module.
--
resolveImports :: ExportEnvironment -> Module -> Either ErrorStack ImportEnvironment
resolveImports env (Module currentModule decls _) =
  foldM resolveImport' (ImportEnvironment M.empty M.empty M.empty M.empty) (M.toList scope)
  where

  -- A Map from module name to the source position for the import, the list of imports from that
  -- module (where Nothing indicates everything is to be imported), and optionally a qualified name
  -- for the module
  scope :: M.Map ModuleName (Maybe SourcePos, Maybe ExplicitImports, Maybe ModuleName)
  scope = M.insert currentModule (Nothing, Nothing, Nothing) (findImports decls)

  resolveImport' :: ImportEnvironment -> (ModuleName, (Maybe SourcePos, Maybe ExplicitImports, Maybe ModuleName)) -> Either ErrorStack ImportEnvironment
  resolveImport' imp (mn, (pos, explImports, impQual)) = do
    modExports <- positioned $ maybe (throwError $ mkErrorStack ("Cannot import unknown module '" ++ show mn ++ "'") Nothing) return $ mn `M.lookup` env
    positioned $ resolveImport currentModule mn modExports imp impQual explImports
    where
    positioned err = case pos of
      Nothing -> err
      Just pos' -> rethrowWithPosition pos' err

-- |
-- Extends the local environment for a module by resolving an import of another module.
--
resolveImport :: ModuleName -> ModuleName -> Exports -> ImportEnvironment -> Maybe ModuleName -> Maybe ExplicitImports-> Either ErrorStack ImportEnvironment
resolveImport currentModule importModule exps imps impQual = maybe importAll (foldM importExplicit imps)
  where

  -- Import everything from a module
  importAll :: Either ErrorStack ImportEnvironment
  importAll = do
    imp' <- foldM (\m (name, dctors) -> importExplicit m (TypeRef name (Just dctors))) imps (exportedTypes exps)
    imp'' <- foldM (\m name -> importExplicit m (ValueRef name)) imp' (exportedValues exps)
    foldM (\m name -> importExplicit m (TypeClassRef name)) imp'' (exportedTypeClasses exps)

  -- Import something explicitly
  importExplicit :: ImportEnvironment -> DeclarationRef -> Either ErrorStack ImportEnvironment
  importExplicit imp (PositionedDeclarationRef pos r) = rethrowWithPosition pos $ importExplicit imp r
  importExplicit imp (ValueRef name) = do
    _ <- checkImportExists "value" values name
    values' <- updateImports (importedValues imp) name
    return $ imp { importedValues = values' }
  importExplicit imp (TypeRef name dctors) = do
    _ <- checkImportExists "type" availableTypes name
    types' <- updateImports (importedTypes imp) name
    let allDctors = allExportedDataConstructors name
    dctors' <- maybe (return allDctors) (mapM $ checkDctorExists allDctors) dctors
    dctors'' <- foldM updateImports (importedDataConstructors imp) dctors'
    return $ imp { importedTypes = types', importedDataConstructors = dctors'' }
  importExplicit imp (TypeClassRef name) = do
    _ <- checkImportExists "type class" classes name
    typeClasses' <- updateImports (importedTypeClasses imp) name
    return $ imp { importedTypeClasses = typeClasses' }
  importExplicit _ _ = error "Invalid argument to importExplicit"

  -- Find all exported data constructors for a given type
  allExportedDataConstructors :: ProperName -> [ProperName]
  allExportedDataConstructors name = fromMaybe [] $ name `lookup` exportedTypes exps

  -- Add something to the ImportEnvironment if it does not already exist there
  updateImports :: (Ord a, Show a) => M.Map (Qualified a) (Qualified a) -> a -> Either ErrorStack (M.Map (Qualified a) (Qualified a))
  updateImports m name = case M.lookup (Qualified impQual name) m of
    Nothing -> return $ M.insert (Qualified impQual name) (Qualified (Just importModule) name) m
    Just (Qualified Nothing _) -> error "Invalid state in updateImports"
    Just x@(Qualified (Just mn) _) -> throwError $ mkErrorStack err Nothing
      where
      err = if mn == currentModule || importModule == currentModule
            then "Definition '" ++ show name ++ "' conflicts with import '" ++ show (Qualified (Just importModule) name) ++ "'"
            else "Conflicting imports for '" ++ show name ++ "': '" ++ show x ++ "', '" ++ show (Qualified (Just importModule) name) ++ "'"

  -- The available values, types, and classes in the module being imported
  values = exportedValues exps
  availableTypes = fst `map` exportedTypes exps
  classes = exportedTypeClasses exps

  -- Ensure that an explicitly imported data constructor exists for the type it is being imported
  -- from
  checkDctorExists :: [ProperName] -> ProperName -> Either ErrorStack ProperName
  checkDctorExists = checkImportExists "data constructor"

  -- Check that an explicitly imported item exists in the module it is being imported from
  checkImportExists :: (Eq a, Show a) => String -> [a] -> a -> Either ErrorStack a
  checkImportExists t exports item =
      if item `elem` exports
      then return item
      else throwError $ mkErrorStack ("Cannot import unknown " ++ t ++  " '" ++ show item ++ "' from '" ++ show importModule ++ "'") Nothing


