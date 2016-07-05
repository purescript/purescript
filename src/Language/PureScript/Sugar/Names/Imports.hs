module Language.PureScript.Sugar.Names.Imports
  ( ImportDef
  , resolveImports
  , resolveModuleImport
  , findImports
  ) where

import Prelude.Compat

import Control.Monad
import Control.Monad.Error.Class (MonadError(..))

import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Sugar.Names.Env

type ImportDef = (Maybe SourceSpan, ImportDeclarationType, Maybe ModuleName)

-- |
-- Finds the imports within a module, mapping the imported module name to an optional set of
-- explicitly imported declarations.
--
findImports
  :: [Declaration]
  -> M.Map ModuleName [ImportDef]
findImports = foldl (go Nothing) M.empty
  where
  go pos result (ImportDeclaration mn typ qual) =
    let imp = (pos, typ, qual)
    in M.insert mn (maybe [imp] (imp :) (mn `M.lookup` result)) result
  go _ result (PositionedDeclaration pos _ d) = go (Just pos) result d
  go _ result _ = result

-- |
-- Constructs a set of imports for a module.
--
resolveImports
  :: forall m
   . MonadError MultipleErrors m
  => Env
  -> Module
  -> m (Module, Imports)
resolveImports env (Module ss coms currentModule decls exps) =
  rethrow (addHint (ErrorInModule currentModule)) $ do
    let imports = findImports decls
        imports' = M.map (map (\(ss', dt, mmn) -> (ss', Just dt, mmn))) imports
        scope = M.insert currentModule [(Nothing, Nothing, Nothing)] imports'
    (Module ss coms currentModule decls exps,) <$>
      foldM (resolveModuleImport env) primImports (M.toList scope)

-- | Constructs a set of imports for a single module import.
resolveModuleImport
  :: forall m
   . MonadError MultipleErrors m
  => Env
  -> Imports
  -> (ModuleName, [(Maybe SourceSpan, Maybe ImportDeclarationType, Maybe ModuleName)])
  -> m Imports
resolveModuleImport env ie (mn, imps) = foldM go ie imps
  where
  go :: Imports
     -> (Maybe SourceSpan, Maybe ImportDeclarationType, Maybe ModuleName)
     -> m Imports
  go ie' (pos, typ, impQual) = do
    modExports <-
      positioned $ maybe
        (throwError . errorMessage . UnknownName . Qualified Nothing $ ModName mn)
        (return . envModuleExports)
        (mn `M.lookup` env)
    let impModules = importedModules ie'
        qualModules = importedQualModules ie'
        ie'' = ie' { importedModules = maybe (S.insert mn impModules) (const impModules) impQual
                   , importedQualModules = maybe qualModules (`S.insert` qualModules) impQual
                   }
    positioned $ resolveImport mn modExports ie'' impQual typ
    where
    positioned err = case pos of
      Nothing -> err
      Just pos' -> rethrowWithPosition pos' err

-- |
-- Extends the local environment for a module by resolving an import of another module.
--
resolveImport
  :: forall m
   . MonadError MultipleErrors m
  => ModuleName
  -> Exports
  -> Imports
  -> Maybe ModuleName
  -> Maybe ImportDeclarationType
  -> m Imports
resolveImport importModule exps imps impQual = resolveByType
  where

  resolveByType :: Maybe ImportDeclarationType -> m Imports
  resolveByType Nothing =
    importAll (importRef Local)
  resolveByType (Just Implicit) =
    importAll (importRef FromImplicit)
  resolveByType (Just (Explicit refs)) =
    checkRefs False refs >> foldM (importRef FromExplicit) imps refs
  resolveByType (Just (Hiding refs)) =
    checkRefs True refs >> importAll (importNonHidden refs)

  -- Check that a 'DeclarationRef' refers to an importable symbol
  checkRefs :: Bool -> [DeclarationRef] -> m ()
  checkRefs isHiding = traverse_ check
    where
    check (PositionedDeclarationRef pos _ r) =
      rethrowWithPosition pos $ check r
    check (ValueRef name) =
      checkImportExists IdentName (exportedValues exps) name
    check (ValueOpRef op) =
      checkImportExists ValOpName (exportedValueOps exps) op
    check (TypeRef name dctors) = do
      checkImportExists TyName (exportedTypes exps) name
      let (allDctors, _) = allExportedDataConstructors name
      for_ dctors $ traverse_ (checkDctorExists name allDctors)
    check (TypeOpRef name) =
      checkImportExists TyOpName (exportedTypeOps exps) name
    check (TypeClassRef name) =
      checkImportExists TyClassName (exportedTypeClasses exps) name
    check (ModuleRef name) | isHiding =
      throwError . errorMessage $ ImportHidingModule name
    check r = internalError $ "Invalid argument to checkRefs: " ++ show r

  -- Check that an explicitly imported item exists in the module it is being imported from
  checkImportExists
    :: Ord a
    => (a -> Name)
    -> M.Map a b
    -> a
    -> m ()
  checkImportExists toName exports item
    = when (item `M.notMember` exports)
    . throwError . errorMessage
    $ UnknownImport importModule (toName item)

  -- Ensure that an explicitly imported data constructor exists for the type it is being imported
  -- from
  checkDctorExists
    :: ProperName 'TypeName
    -> [ProperName 'ConstructorName]
    -> ProperName 'ConstructorName
    -> m ()
  checkDctorExists tcon exports dctor
    = when (dctor `notElem` exports)
    . throwError . errorMessage
    $ UnknownImportDataConstructor importModule tcon dctor

  importNonHidden :: [DeclarationRef] -> Imports -> DeclarationRef -> m Imports
  importNonHidden hidden m ref | isHidden ref = return m
                               | otherwise = importRef FromImplicit m ref
    where
    -- TODO: rework this to be not confusing
    isHidden :: DeclarationRef -> Bool
    isHidden ref'@(TypeRef _ _) = foldl (checkTypeRef ref') False hidden
    isHidden ref' = ref' `elem` hidden
    checkTypeRef :: DeclarationRef -> Bool -> DeclarationRef -> Bool
    checkTypeRef _ True _ = True
    checkTypeRef r acc (PositionedDeclarationRef _ _ h) = checkTypeRef r acc h
    checkTypeRef (TypeRef _ Nothing) acc (TypeRef _ (Just _)) = acc
    checkTypeRef (TypeRef name (Just dctor)) _ (TypeRef name' (Just dctor')) = name == name' && dctor == dctor'
    checkTypeRef (TypeRef name _) _ (TypeRef name' Nothing) = name == name'
    checkTypeRef (PositionedDeclarationRef _ _ r) acc hiddenRef = checkTypeRef r acc hiddenRef
    checkTypeRef _ acc _ = acc

  -- Import all symbols
  importAll :: (Imports -> DeclarationRef -> m Imports) -> m Imports
  importAll importer =
    foldM (\m (name, (dctors, _)) -> importer m (TypeRef name (Just dctors))) imps (M.toList (exportedTypes exps))
      >>= flip (foldM (\m (name, _) -> importer m (TypeOpRef name))) (M.toList (exportedTypeOps exps))
      >>= flip (foldM (\m (name, _) -> importer m (ValueRef name))) (M.toList (exportedValues exps))
      >>= flip (foldM (\m (name, _) -> importer m (ValueOpRef name))) (M.toList (exportedValueOps exps))
      >>= flip (foldM (\m (name, _) -> importer m (TypeClassRef name))) (M.toList (exportedTypeClasses exps))

  importRef :: ImportProvenance -> Imports -> DeclarationRef -> m Imports
  importRef prov imp (PositionedDeclarationRef pos _ r) =
    rethrowWithPosition pos $ importRef prov imp r
  importRef prov imp (ValueRef name) = do
    let values' = updateImports (importedValues imp) (exportedValues exps) id name prov
    return $ imp { importedValues = values' }
  importRef prov imp (ValueOpRef name) = do
    let valueOps' = updateImports (importedValueOps imp) (exportedValueOps exps) id name prov
    return $ imp { importedValueOps = valueOps' }
  importRef prov imp (TypeRef name dctors) = do
    let types' = updateImports (importedTypes imp) (exportedTypes exps) snd name prov
    let (dctorNames, mn) = allExportedDataConstructors name
        dctorLookup :: M.Map (ProperName 'ConstructorName) ModuleName
        dctorLookup = M.fromList $ map (, mn) dctorNames
    traverse_ (traverse_ $ checkDctorExists name dctorNames) dctors
    let dctors' = foldl (\m d -> updateImports m dctorLookup id d prov) (importedDataConstructors imp) (fromMaybe dctorNames dctors)
    return $ imp { importedTypes = types', importedDataConstructors = dctors' }
  importRef prov imp (TypeOpRef name) = do
    let ops' = updateImports (importedTypeOps imp) (exportedTypeOps exps) id name prov
    return $ imp { importedTypeOps = ops' }
  importRef prov imp (TypeClassRef name) = do
    let typeClasses' = updateImports (importedTypeClasses imp) (exportedTypeClasses exps) id name prov
    return $ imp { importedTypeClasses = typeClasses' }
  importRef _ _ TypeInstanceRef{} = internalError "TypeInstanceRef in importRef"
  importRef _ _ ModuleRef{} = internalError "ModuleRef in importRef"
  importRef _ _ ReExportRef{} = internalError "ReExportRef in importRef"

  -- Find all exported data constructors for a given type
  allExportedDataConstructors
    :: ProperName 'TypeName
    -> ([ProperName 'ConstructorName], ModuleName)
  allExportedDataConstructors name =
    fromMaybe (internalError "Invalid state in allExportedDataConstructors")
      $ name `M.lookup` exportedTypes exps

  -- Add something to an import resolution list
  updateImports
    :: Ord a
    => M.Map (Qualified a) [ImportRecord a]
    -> M.Map a b
    -> (b -> ModuleName)
    -> a
    -> ImportProvenance
    -> M.Map (Qualified a) [ImportRecord a]
  updateImports imps' exps' expName name prov =
    let
      mnOrig = maybe (internalError "Invalid state in updateImports") expName (name `M.lookup` exps')
      rec = ImportRecord (Qualified (Just importModule) name) mnOrig prov
    in
      M.alter
        (\currNames -> Just $ rec : fromMaybe [] currNames)
        (Qualified impQual name)
        imps'
