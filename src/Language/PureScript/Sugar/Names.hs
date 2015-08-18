-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Names
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}

module Language.PureScript.Sugar.Names (
  desugarImports
) where

import Data.List (find, nub, (\\))
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)

import Control.Applicative (Applicative(..), (<$>), (<*>))
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer (MonadWriter(..), censor)

import qualified Data.Map as M

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Traversals

import qualified Language.PureScript.Constants as C

-- |
-- The global export environment - every declaration exported from every module.
--
type ExportEnvironment = M.Map ModuleName Exports

-- |
-- The exported declarations from a module.
--
data Exports = Exports
  {
  -- |
  -- The filename of the module these exports are for
  --
    moduleSourceSpan :: SourceSpan
  -- |
  -- The types exported from each module
  --
  , exportedTypes :: [(ProperName, [ProperName])]
  -- |
  -- The classes exported from each module
  --
  , exportedTypeClasses :: [ProperName]
  -- |
  -- The values exported from each module
  --
  , exportedValues :: [Ident]
  -- |
  -- The modules exported from each module
  --
  , exportedModules :: [ModuleName]
  } deriving (Show)

-- |
-- An imported environment for a particular module. This also contains the module's own members.
--
data ImportEnvironment = ImportEnvironment
  {
  -- |
  -- Local names for types within a module mapped to to their qualified names
  --
    importedTypes :: M.Map (Qualified ProperName) (Qualified ProperName)
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
updateExportedModule :: (Applicative m, MonadError MultipleErrors m) => ExportEnvironment -> ModuleName -> (Exports -> m Exports) -> m ExportEnvironment
updateExportedModule env mn update = do
  let exports = fromMaybe (error "Module was undefined in updateExportedModule") $ mn `M.lookup` env
  exports' <- update exports
  return $ M.insert mn exports' env

-- |
-- Adds an empty module to an ExportEnvironment.
--
addEmptyModule :: (Applicative m, MonadError MultipleErrors m) => ExportEnvironment -> ModuleName -> SourceSpan -> m ExportEnvironment
addEmptyModule env name ss =
  case name `M.lookup` env of
    Just m -> throwError . errorMessage $ RedefinedModule name [moduleSourceSpan m, ss]
    Nothing -> return $ M.insert name (Exports ss [] [] [] []) env

-- |
-- Adds a type belonging to a module to the export environment.
--
addType :: (Applicative m, MonadError MultipleErrors m) => ExportEnvironment -> ModuleName -> ProperName -> [ProperName] -> m ExportEnvironment
addType env mn name dctors = updateExportedModule env mn $ \m -> do
  let exTypes = exportedTypes m
  let exDctors = snd `concatMap` exTypes
  let exClasses = exportedTypeClasses m
  when (any ((== name) . fst) exTypes) $ throwConflictError ConflictingTypeDecls name
  when (name `elem` exClasses) $ throwConflictError TypeConflictsWithClass name
  forM_ dctors $ \dctor -> do
    when (dctor `elem` exDctors) $ throwConflictError ConflictingCtorDecls dctor
    when (dctor `elem` exClasses) $ throwConflictError CtorConflictsWithClass dctor
  return $ m { exportedTypes = (name, dctors) : exTypes }

-- |
-- Adds a class to the export environment.
--
addTypeClass :: (Applicative m, MonadError MultipleErrors m) => ExportEnvironment -> ModuleName -> ProperName -> m ExportEnvironment
addTypeClass env mn name = updateExportedModule env mn $ \m -> do
  let exTypes = exportedTypes m
  let exDctors = snd `concatMap` exTypes
  when (any ((== name) . fst) exTypes) $ throwConflictError ClassConflictsWithType name
  when (name `elem` exDctors) $ throwConflictError ClassConflictsWithCtor name
  classes <- addExport DuplicateClassExport (exportedTypeClasses m) name
  return $ m { exportedTypeClasses = classes }

-- |
-- Adds a class to the export environment.
--
addValue :: (Applicative m, MonadError MultipleErrors m) => ExportEnvironment -> ModuleName -> Ident -> m ExportEnvironment
addValue env mn name = updateExportedModule env mn $ \m -> do
  values <- addExport DuplicateValueExport (exportedValues m) name
  return $ m { exportedValues = values }

-- |
-- Adds an entry to a list of exports unless it is already present, in which case an error is
-- returned.
--
addExport :: (Applicative m, MonadError MultipleErrors m, Eq a, Show a) => (a -> SimpleErrorMessage) -> [a] -> a -> m [a]
addExport what exports name =
  if name `elem` exports
  then throwConflictError what name
  else return $ name : exports

-- |
-- Replaces all local names with qualified names within a set of modules.
--
desugarImports :: forall m. (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) => [Module] -> m [Module]
desugarImports modules = do
  unfilteredExports <- findExports modules
  exports <- foldM filterModuleExports unfilteredExports modules
  let modules' = moduleReexports <$> modules
  mapM (renameInModule' unfilteredExports exports) modules'
  where
  moduleReexports :: Module -> Module
  moduleReexports (Module ss coms mn decls exps) =
    let importedMods = catMaybes findImports'
    in (Module ss coms mn (decls ++ (concatMap reexports importedMods)) exps)
    where
    imports :: [Declaration]
    imports = filter isImportDecl decls
    findImports' :: [Maybe (Declaration, Module)]
    findImports' = go <$> modules
      where
      go :: Module -> Maybe (Declaration, Module)
      go m@(Module _ _ mn' _ (Just exps'))
        | any isModExport exps', Just d <- find ((== mn') . importedModName) imports = Just (d, m)
        where
        importedModName :: Declaration -> ModuleName
        importedModName (ImportDeclaration imn _ _) = imn
        importedModName (PositionedDeclaration _ _ d) = importedModName d
        importedModName _ = error "Not an import decl"
      go _ = Nothing
    reexports :: (Declaration, Module) -> [Declaration]
    reexports (ImportDeclaration _ (Hiding refs) _, (Module ss' coms' mn' ds' (Just exps'))) =
      case nonHiddenRefs of
        [] -> []
        _ -> reexports (ImportDeclaration mn' Implicit Nothing, Module ss' coms' mn' ds' (Just nonHiddenRefs))
      where
      nonHiddenRefs :: [DeclarationRef]
      nonHiddenRefs = filter isModExport exps' \\ filter isModExport refs
    reexports (ImportDeclaration _ ty qual, Module _ _ _ _ (Just exps')) =
      let ty' = case ty of
                  Explicit _ -> Explicit []
                  _ -> ty
      in (\m -> ImportDeclaration m ty' qual) <$> (catMaybes $ go <$> exps')
      where
      go :: DeclarationRef -> Maybe ModuleName
      go (ModuleRef mn') = Just mn'
      go _ = Nothing
    reexports (PositionedDeclaration _ _ d, m@(Module _ _ _ _ (Just _))) = reexports (d, m)
    reexports _ = []

  isModExport :: DeclarationRef -> Bool
  isModExport (ModuleRef _) = True
  isModExport _ = False

  -- Filters the exports for a module in the global exports environment so that only explicitly
  -- exported members remain. If the module does not explicitly export anything, everything is
  -- exported.
  filterModuleExports :: ExportEnvironment -> Module -> m ExportEnvironment
  filterModuleExports env (Module _ _ mn _ (Just exps))
    | any isSelfModuleExport exps, Just exps' <- M.lookup mn env =
        let moduleNames = filter (/= mn) $ (\(ModuleRef mn') -> mn') <$> filter isModExport exps
        in return $ M.insert mn (exps' {exportedModules = moduleNames}) env
    where
    isSelfModuleExport :: DeclarationRef -> Bool
    isSelfModuleExport (ModuleRef mn') | mn' == mn = True
    isSelfModuleExport (PositionedDeclarationRef _ _ ref) = isSelfModuleExport ref
    isSelfModuleExport _ = False
  filterModuleExports env (Module _ _ mn _ (Just exps)) = filterExports mn exps env
  filterModuleExports env _ = return env

  -- Rename and check all the names within a module. We tweak the global exports environment so
  -- the module has access to an unfiltered list of its own members.
  renameInModule' :: ExportEnvironment -> ExportEnvironment -> Module -> m Module
  renameInModule' unfilteredExports exports m@(Module _ _ mn _ _) =
    rethrow (onErrorMessages (ErrorInModule mn)) $ do
      let env = M.update (\_ -> M.lookup mn unfilteredExports) mn exports
      let exps = fromMaybe (error "Module is missing in renameInModule'") $ M.lookup mn exports
      imports <- resolveImports env m
      elaborateImports <$> renameInModule imports env (elaborateExports exps m)

-- |
-- Make all exports for a module explicit. This may still effect modules that have an exports list,
-- as it will also make all data constructor exports explicit.
--
elaborateExports :: Exports -> Module -> Module
elaborateExports exps (Module ss coms mn decls _) = Module ss coms mn decls (Just $
  map (\(ctor, dctors) -> TypeRef ctor (Just dctors)) (exportedTypes exps) ++
  map TypeClassRef (exportedTypeClasses exps) ++
  map ValueRef (exportedValues exps) ++
  map ModuleRef (exportedModules exps))

-- |
-- Add `import X ()` for any modules where there are only fully qualified references to members.
-- This ensures transitive instances are included when using a member from a module.
--
elaborateImports :: Module -> Module
elaborateImports (Module ss coms mn decls exps) = Module ss coms mn decls' exps
  where
  decls' :: [Declaration]
  decls' =
    let (f, _, _, _, _) = everythingOnValues (++) (const []) fqValues (const []) (const []) (const [])
    in mkImport `map` nub (f `concatMap` decls) ++ decls
  fqValues :: Expr -> [ModuleName]
  fqValues (Var (Qualified (Just mn') _)) = [mn']
  fqValues _ = []
  mkImport :: ModuleName -> Declaration
  mkImport mn' = ImportDeclaration mn' (Explicit []) Nothing

-- |
-- Replaces all local names with qualified names within a module and checks that all existing
-- qualified names are valid.
--
renameInModule :: forall m. (Applicative m, MonadError MultipleErrors m) => ImportEnvironment -> ExportEnvironment -> Module -> m Module
renameInModule imports exports (Module ss coms mn decls exps) =
  Module ss coms mn <$> parU decls go <*> pure exps
  where
  (go, _, _, _, _) = everywhereWithContextOnValuesM (Nothing, []) updateDecl updateValue updateBinder updateCase defS

  updateDecl :: (Maybe SourceSpan, [Ident]) -> Declaration -> m ((Maybe SourceSpan, [Ident]), Declaration)
  updateDecl (_, bound) d@(PositionedDeclaration pos _ _) = return ((Just pos, bound), d)
  updateDecl (pos, bound) (DataDeclaration dtype name args dctors) =
    (,) (pos, bound) <$> (DataDeclaration dtype name args <$> mapM (sndM (mapM (updateTypesEverywhere pos))) dctors)
  updateDecl (pos, bound) (TypeSynonymDeclaration name ps ty) =
    (,) (pos, bound) <$> (TypeSynonymDeclaration name ps <$> updateTypesEverywhere pos ty)
  updateDecl (pos, bound) (TypeClassDeclaration className args implies ds) =
    (,) (pos, bound) <$> (TypeClassDeclaration className args <$> updateConstraints pos implies <*> pure ds)
  updateDecl (pos, bound) (TypeInstanceDeclaration name cs cn ts ds) =
    (,) (pos, bound) <$> (TypeInstanceDeclaration name <$> updateConstraints pos cs <*> updateClassName cn pos <*> mapM (updateTypesEverywhere pos) ts <*> pure ds)
  updateDecl (pos, bound) (ExternInstanceDeclaration name cs cn ts) =
    (,) (pos, bound) <$> (ExternInstanceDeclaration name <$> updateConstraints pos cs <*> updateClassName cn Nothing <*> mapM (updateTypesEverywhere pos) ts)
  updateDecl (pos, bound) (TypeDeclaration name ty) =
    (,) (pos, bound) <$> (TypeDeclaration name <$> updateTypesEverywhere pos ty)
  updateDecl (pos, bound) (ExternDeclaration name ty) =
    (,) (pos, name : bound) <$> (ExternDeclaration name <$> updateTypesEverywhere pos ty)
  updateDecl s d = return (s, d)

  updateValue :: (Maybe SourceSpan, [Ident]) -> Expr -> m ((Maybe SourceSpan, [Ident]), Expr)
  updateValue (_, bound) v@(PositionedValue pos' _ _) = return ((Just pos', bound), v)
  updateValue (pos, bound) (Abs (Left arg) val') = return ((pos, arg : bound), Abs (Left arg) val')
  updateValue (pos, bound) (Let ds val') = do
      let args = mapMaybe letBoundVariable ds
      unless (length (nub args) == length args) $
        maybe id rethrowWithPosition pos $
          throwError . errorMessage $ OverlappingNamesInLet
      return ((pos, args ++ bound), Let ds val')
      where
  updateValue (pos, bound) (Var name'@(Qualified Nothing ident)) | ident `notElem` bound =
    (,) (pos, bound) <$> (Var <$> updateValueName name' pos)
  updateValue (pos, bound) (Var name'@(Qualified (Just _) _)) =
    (,) (pos, bound) <$> (Var <$> updateValueName name' pos)
  updateValue s@(pos, _) (Constructor name) = (,) s <$> (Constructor <$> updateDataConstructorName name pos)
  updateValue s@(pos, _) (TypedValue check val ty) = (,) s <$> (TypedValue check val <$> updateTypesEverywhere pos ty)
  updateValue s v = return (s, v)

  updateBinder :: (Maybe SourceSpan, [Ident]) -> Binder -> m ((Maybe SourceSpan, [Ident]), Binder)
  updateBinder (_, bound) v@(PositionedBinder pos _ _) = return ((Just pos, bound), v)
  updateBinder s@(pos, _) (ConstructorBinder name b) = (,) s <$> (ConstructorBinder <$> updateDataConstructorName name pos <*> pure b)
  updateBinder s v = return (s, v)

  updateCase :: (Maybe SourceSpan, [Ident]) -> CaseAlternative -> m ((Maybe SourceSpan, [Ident]), CaseAlternative)
  updateCase (pos, bound) c@(CaseAlternative bs _) = return ((pos, concatMap binderNames bs ++ bound), c)

  letBoundVariable :: Declaration -> Maybe Ident
  letBoundVariable (ValueDeclaration ident _ _ _) = Just ident
  letBoundVariable (PositionedDeclaration _ _ d) = letBoundVariable d
  letBoundVariable _ = Nothing

  updateTypesEverywhere :: Maybe SourceSpan -> Type -> m Type
  updateTypesEverywhere pos0 = everywhereOnTypesM (updateType pos0)
    where
    updateType :: Maybe SourceSpan -> Type -> m Type
    updateType pos (TypeConstructor name) = TypeConstructor <$> updateTypeName name pos
    updateType pos (SaturatedTypeSynonym name tys) = SaturatedTypeSynonym <$> updateTypeName name pos <*> pure tys
    updateType pos (ConstrainedType cs t) = ConstrainedType <$> updateConstraints pos cs <*> pure t
    updateType _ t = return t

  updateConstraints pos = mapM (\(name, ts) -> (,) <$> updateClassName name pos <*> mapM (updateTypesEverywhere pos) ts)

  updateTypeName = update UnknownType importedTypes (\mes -> isJust . (`lookup` exportedTypes mes))
  updateClassName = update UnknownTypeClass importedTypeClasses (flip elem . exportedTypeClasses)
  updateValueName = update UnknownValue importedValues (flip elem . exportedValues)
  updateDataConstructorName = update (flip UnknownDataConstructor Nothing) importedDataConstructors (\mes -> flip elem (join $ snd `map` exportedTypes mes))

  -- Update names so unqualified references become qualified, and locally qualified references
  -- are replaced with their canoncial qualified names (e.g. M.Map -> Data.Map.Map)
  update :: (Ord a, Show a) => (Qualified a -> SimpleErrorMessage)
                            -> (ImportEnvironment -> M.Map (Qualified a) (Qualified a))
                            -> (Exports -> a -> Bool)
                            -> Qualified a
                            -> Maybe SourceSpan
                            -> m (Qualified a)
  update unknown getI checkE qname@(Qualified mn' name) pos = positioned $ case (M.lookup qname imports', mn') of
    (Just qname', _) -> return qname'
    (Nothing, Just mn'') -> do
      when (isExplicitQualModule mn'') . throwError . errorMessage $ unknown qname
      modExports <- getExports mn''
      if checkE modExports name
        then return qname
        else throwError . errorMessage $ unknown qname
    _ -> throwError . errorMessage $ unknown qname
    where
    isExplicitQualModule :: ModuleName -> Bool
    isExplicitQualModule = flip elem $ mapMaybe (\(Qualified q _) -> q) (M.keys imports')
    imports' = getI imports
    positioned err = case pos of
      Nothing -> err
      Just pos' -> rethrowWithPosition pos' err

  -- Gets the exports for a module, or an error message if the module doesn't exist
  getExports :: ModuleName -> m Exports
  getExports mn' = maybe (throwError . errorMessage $ UnknownModule mn') return $ M.lookup mn' exports

-- |
-- Finds all exported declarations in a set of modules.
--
findExports :: forall m. (Applicative m, MonadError MultipleErrors m) => [Module] -> m ExportEnvironment
findExports = foldM addModule $ M.singleton (ModuleName [ProperName C.prim]) primExports
  where

  -- The exported types from the Prim module
  primExports = Exports (internalModuleSourceSpan "<Prim>") (mkTypeEntry `map` M.keys primTypes) [] [] []
    where
    mkTypeEntry (Qualified _ name) = (name, [])

  -- Add all of the exported declarations from a module to the global export environment
  addModule :: ExportEnvironment -> Module -> m ExportEnvironment
  addModule env (Module ss _ mn ds _) = do
    env' <- addEmptyModule env mn ss
    rethrow (onErrorMessages (ErrorInModule mn)) $ foldM (addDecl mn) env' ds

  -- Add a declaration from a module to the global export environment
  addDecl :: ModuleName -> ExportEnvironment -> Declaration -> m ExportEnvironment
  addDecl mn env (TypeClassDeclaration tcn _ _ ds) = do
    env' <- addTypeClass env mn tcn
    foldM go env' ds
    where
    go env'' (TypeDeclaration name _) = addValue env'' mn name
    go env'' (PositionedDeclaration pos _ d) = rethrowWithPosition pos $ go env'' d
    go _ _ = error "Invalid declaration in TypeClassDeclaration"
  addDecl mn env (DataDeclaration _ tn _ dcs) = addType env mn tn (map fst dcs)
  addDecl mn env (TypeSynonymDeclaration tn _ _) = addType env mn tn []
  addDecl mn env (ExternDataDeclaration tn _) = addType env mn tn []
  addDecl mn env (ValueDeclaration name _ _ _) = addValue env mn name
  addDecl mn env (ExternDeclaration name _) = addValue env mn name
  addDecl mn env (PositionedDeclaration pos _ d) = rethrowWithPosition pos $ addDecl mn env d
  addDecl _  env _ = return env

-- |
-- Filters the exports for a module to ensure only explicit exports are kept in the global exports
-- environment.
--
filterExports :: forall m. (Applicative m, MonadError MultipleErrors m) => ModuleName -> [DeclarationRef] -> ExportEnvironment -> m ExportEnvironment
filterExports mn exps env = do
  let moduleExports = fromMaybe (error "Module is missing") (mn `M.lookup` env)
  moduleExports' <- rethrow (onErrorMessages (ErrorInModule mn)) $ filterModule moduleExports
  return $ M.insert mn moduleExports' env
  where

  -- Filter the exports for the specific module
  filterModule :: Exports -> m Exports
  filterModule exported = do
    types' <- foldM (filterTypes $ exportedTypes exported) [] exps
    values <- foldM (filterValues $ exportedValues exported) [] exps
    classes <- foldM (filterClasses $ exportedTypeClasses exported) [] exps
    modules <- foldM (filterModules $ exportedModules exported) [] exps
    return exported { exportedTypes = types', exportedTypeClasses = classes, exportedValues = values, exportedModules = modules }

  -- Ensure the exported types and data constructors exist in the module and add them to the set of
  -- exports
  filterTypes :: [(ProperName, [ProperName])] -> [(ProperName, [ProperName])] -> DeclarationRef -> m [(ProperName, [ProperName])]
  filterTypes expTys result (PositionedDeclarationRef pos _ r) = rethrowWithPosition pos $ filterTypes expTys result r
  filterTypes expTys result (TypeRef name expDcons) = do
    dcons <- maybe (throwError . errorMessage . UnknownExportType $ name) return $ name `lookup` expTys
    dcons' <- maybe (return dcons) (foldM (filterDcons name dcons) []) expDcons
    return $ (name, dcons') : result
  filterTypes _ result _ = return result

  -- Ensure the exported data constructors exists for a type and add them to the list of exports
  filterDcons :: ProperName -> [ProperName] -> [ProperName] -> ProperName -> m [ProperName]
  filterDcons tcon exps' result name =
    if name `elem` exps'
    then return $ name : result
    else throwError . errorMessage $ UnknownExportDataConstructor tcon name

  -- Ensure the exported classes exist in the module and add them to the set of exports
  filterClasses :: [ProperName] -> [ProperName] -> DeclarationRef -> m [ProperName]
  filterClasses exps' result (PositionedDeclarationRef pos _ r) = rethrowWithPosition pos $ filterClasses exps' result r
  filterClasses exps' result (TypeClassRef name) =
    if name `elem` exps'
    then return $ name : result
    else throwError . errorMessage . UnknownExportTypeClass $ name
  filterClasses _ result _ = return result

  -- Ensure the exported values exist in the module and add them to the set of exports
  filterValues :: [Ident] -> [Ident] -> DeclarationRef -> m [Ident]
  filterValues exps' result (PositionedDeclarationRef pos _ r) = rethrowWithPosition pos $ filterValues exps' result r
  filterValues exps' result (ValueRef name) =
    if name `elem` exps'
    then return $ name : result
    else throwError . errorMessage . UnknownExportValue $ name
  filterValues _ result _ = return result

  -- Add the exported modules to the set of exports
  filterModules :: [ModuleName] -> [ModuleName] -> DeclarationRef -> m [ModuleName]
  filterModules exps' result (PositionedDeclarationRef pos _ r) = rethrowWithPosition pos $ filterModules exps' result r
  filterModules _ result (ModuleRef name) = return $ name : result
  filterModules _ result _ = return result

-- |
-- Finds the imports within a module, mapping the imported module name to an optional set of
-- explicitly imported declarations.
--
findImports :: [Declaration] -> M.Map ModuleName [(Maybe SourceSpan, ImportDeclarationType, Maybe ModuleName)]
findImports = foldl (findImports' Nothing) M.empty
  where
  findImports' pos result (ImportDeclaration mn typ qual) =
    case mn `M.lookup` result of
      Just is -> M.insert mn ((pos, typ, qual):is) result
      Nothing -> M.insert mn [(pos, typ, qual)] result
  findImports' _ result (PositionedDeclaration pos _ d) = findImports' (Just pos) result d
  findImports' _ result _ = result

-- |
-- Constructs a local environment for a module.
--
resolveImports :: forall m. (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) => ExportEnvironment -> Module -> m ImportEnvironment
resolveImports env (Module _ _ currentModule decls _) =
  censor (onErrorMessages (ErrorInModule currentModule)) $
    foldM resolveImport' (ImportEnvironment M.empty M.empty M.empty M.empty) (M.toList scope)
  where

  -- A Map from module name to the source position for the import, the list of imports from that
  -- module (where Nothing indicates everything is to be imported), and optionally a qualified name
  -- for the module
  scope :: M.Map ModuleName [(Maybe SourceSpan, ImportDeclarationType, Maybe ModuleName)]
  scope = M.insert currentModule [(Nothing, Implicit, Nothing)] (findImports decls)

  resolveImport' :: ImportEnvironment -> (ModuleName, [(Maybe SourceSpan, ImportDeclarationType, Maybe ModuleName)]) -> m ImportEnvironment
  resolveImport' ie (mn, imps) = foldM go ie imps
    where
    go :: ImportEnvironment -> (Maybe SourceSpan, ImportDeclarationType, Maybe ModuleName) -> m ImportEnvironment
    go ie' (pos, typ, impQual) = do
      modExports <- positioned $ maybe (throwError . errorMessage $ UnknownModule mn) return $ mn `M.lookup` env
      positioned $ resolveImport currentModule mn modExports ie' impQual typ
      where
      positioned err = case pos of
        Nothing -> err
        Just pos' -> rethrowWithPosition pos' err

-- |
-- Extends the local environment for a module by resolving an import of another module.
--
resolveImport :: forall m. (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) => ModuleName -> ModuleName -> Exports -> ImportEnvironment -> Maybe ModuleName -> ImportDeclarationType -> m ImportEnvironment
resolveImport currentModule importModule exps imps impQual =
  resolveByType
  where

  resolveByType :: ImportDeclarationType -> m ImportEnvironment
  resolveByType Implicit = importAll importExplicit
  resolveByType (Explicit explImports) = checkedRefs explImports >> foldM importExplicit imps explImports
  resolveByType (Hiding hiddenImports) = checkedRefs hiddenImports >> importAll (importNonHidden hiddenImports)

  importNonHidden :: [DeclarationRef] -> ImportEnvironment -> DeclarationRef -> m ImportEnvironment
  importNonHidden hidden m ref =
    if isHidden hidden ref
    then return m
    else importExplicit m ref

  isHidden :: [DeclarationRef] -> DeclarationRef -> Bool
  isHidden hidden ref@(TypeRef _ _) =
    let
      checkTypeRef _ True _ = True
      checkTypeRef r acc (PositionedDeclarationRef _ _ h) = checkTypeRef r acc h
      checkTypeRef (TypeRef _ Nothing) acc (TypeRef _ (Just _)) = acc
      checkTypeRef (TypeRef name (Just dctor)) _ (TypeRef name' (Just dctor')) = name == name' && dctor == dctor'
      checkTypeRef (TypeRef name _) _ (TypeRef name' Nothing) = name == name'
      checkTypeRef (PositionedDeclarationRef _ _ r) acc hiddenRef = checkTypeRef r acc hiddenRef
      checkTypeRef _ acc _ = acc
    in foldl (checkTypeRef ref) False hidden
  isHidden hidden ref = ref `elem` hidden

  -- Import all symbols
  importAll :: (ImportEnvironment -> DeclarationRef -> m ImportEnvironment) -> m ImportEnvironment
  importAll importer = do
    imp' <- foldM (\m (name, dctors) -> importer m (TypeRef name (Just dctors))) imps (exportedTypes exps)
    imp'' <- foldM (\m name -> importer m (ValueRef name)) imp' (exportedValues exps)
    foldM (\m name -> importer m (TypeClassRef name)) imp'' (exportedTypeClasses exps)

  -- Import something explicitly
  importExplicit :: ImportEnvironment -> DeclarationRef -> m ImportEnvironment
  importExplicit imp (PositionedDeclarationRef pos _ r) =
    rethrowWithPosition pos . warnWithPosition pos $ importExplicit imp r
  importExplicit imp (ValueRef name) = do
    values' <- updateImports (importedValues imp) name
    return $ imp { importedValues = values' }
  importExplicit imp (TypeRef name dctors) = do
    types' <- updateImports (importedTypes imp) name
    let allDctors = allExportedDataConstructors name
    maybe (return ()) (mapM_ $ checkDctorExists name allDctors) dctors
    when (null allDctors && isNothing dctors) . tell . errorMessage $ MisleadingEmptyTypeImport importModule name
    dctors' <- foldM updateImports (importedDataConstructors imp) (fromMaybe allDctors dctors)
    return $ imp { importedTypes = types', importedDataConstructors = dctors' }
  importExplicit imp (TypeClassRef name) = do
    typeClasses' <- updateImports (importedTypeClasses imp) name
    return $ imp { importedTypeClasses = typeClasses' }
  importExplicit _ _ = error "Invalid argument to importExplicit"

  -- Check if DeclarationRef points to an existent symbol
  checkedRefs :: [DeclarationRef] -> m ()
  checkedRefs = mapM_ check
    where
    check (PositionedDeclarationRef pos _ r) =
      rethrowWithPosition pos $ check r
    check (ValueRef name) =
      checkImportExists UnknownImportValue values name
    check (TypeRef name dctors) = do
      checkImportExists UnknownImportType availableTypes name
      let allDctors = allExportedDataConstructors name
      maybe (return ()) (mapM_ $ checkDctorExists name allDctors) dctors
    check (TypeClassRef name) =
      checkImportExists UnknownImportTypeClass classes name
    check (ModuleRef name) =
      checkImportExists (const UnknownModule) (exportedModules exps) name
    check _ = error "Invalid argument to checkRefIsValid"

  -- Find all exported data constructors for a given type
  allExportedDataConstructors :: ProperName -> [ProperName]
  allExportedDataConstructors name = fromMaybe [] $ name `lookup` exportedTypes exps

  -- Add something to the ImportEnvironment if it does not already exist there
  updateImports :: (Ord a, Show a) => M.Map (Qualified a) (Qualified a) -> a -> m (M.Map (Qualified a) (Qualified a))
  updateImports m name = case M.lookup (Qualified impQual name) m of
    Nothing -> return $ M.insert (Qualified impQual name) (Qualified (Just importModule) name) m
    Just (Qualified Nothing _) -> error "Invalid state in updateImports"
    Just (Qualified (Just mn) _)
      | mn == importModule -> return m
      | otherwise -> throwError . errorMessage $ err
        where
        err = if currentModule `elem` [mn, importModule]
              then ConflictingImport (show name) importModule
              else ConflictingImports (show name) mn importModule


  -- The available values, types, and classes in the module being imported
  values = exportedValues exps
  availableTypes = fst `map` exportedTypes exps
  classes = exportedTypeClasses exps

  -- Ensure that an explicitly imported data constructor exists for the type it is being imported
  -- from
  checkDctorExists :: ProperName -> [ProperName] -> ProperName -> m ()
  checkDctorExists tcon = checkImportExists (flip UnknownImportDataConstructor tcon)

  -- Check that an explicitly imported item exists in the module it is being imported from
  checkImportExists :: (Eq a, Show a) => (ModuleName -> a -> SimpleErrorMessage) -> [a] -> a -> m ()
  checkImportExists unknown exports item =
    when (item `notElem` exports) $ throwError . errorMessage $ unknown importModule item

-- |
-- Raises an error for when there is more than one definition for something.
--
throwConflictError :: (Applicative m, MonadError MultipleErrors m, Show a) => (a -> SimpleErrorMessage) -> a -> m b
throwConflictError conflict = throwError . errorMessage . conflict
