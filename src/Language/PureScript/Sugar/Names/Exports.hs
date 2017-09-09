module Language.PureScript.Sugar.Names.Exports
  ( findExportable
  , resolveExports
  ) where

import Prelude.Compat

import Control.Monad
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Error.Class (MonadError(..))

import Data.Function (on)
import Data.Foldable (traverse_)
import Data.List (intersect, groupBy, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as M

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Sugar.Names.Env
import Language.PureScript.Sugar.Names.Common (warnDuplicateRefs)

-- |
-- Finds all exportable members of a module, disregarding any explicit exports.
--
findExportable :: forall m. (MonadError MultipleErrors m) => Module -> m Exports
findExportable (Module _ _ mn ds _) =
  rethrow (addHint (ErrorInModule mn)) $ foldM updateExports' nullExports ds
  where
  updateExports' :: Exports -> Declaration -> m Exports
  updateExports' exps decl = rethrowWithPosition (declSourceSpan decl) $ updateExports exps decl

  updateExports :: Exports -> Declaration -> m Exports
  updateExports exps (TypeClassDeclaration (ss, _) tcn _ _ _ ds') = do
    exps' <- rethrowWithPosition ss $ exportTypeClass Internal exps tcn mn
    foldM go exps' ds'
    where
    go exps'' (TypeDeclaration (TypeDeclarationData (ss', _) name _)) = rethrowWithPosition ss' $ exportValue exps'' name mn
    go _ _ = internalError "Invalid declaration in TypeClassDeclaration"
  updateExports exps (DataDeclaration _ _ tn _ dcs) =
    exportType Internal exps tn (map fst dcs) mn
  updateExports exps (TypeSynonymDeclaration _ tn _ _) =
    exportType Internal exps tn [] mn
  updateExports exps (ExternDataDeclaration _ tn _) =
    exportType Internal exps tn [] mn
  updateExports exps (ValueDeclaration vd) =
    exportValue exps (valdeclIdent vd) mn
  updateExports exps (ValueFixityDeclaration _ _ _ op) =
    exportValueOp exps op mn
  updateExports exps (TypeFixityDeclaration _ _ _ op) =
    exportTypeOp exps op mn
  updateExports exps (ExternDeclaration _ name _) =
    exportValue exps name mn
  updateExports exps (ExternKindDeclaration _ pn) =
    exportKind exps pn mn
  updateExports exps _ = return exps

-- |
-- Resolves the exports for a module, filtering out members that have not been
-- exported and elaborating re-exports of other modules.
--
resolveExports
  :: forall m
   . (MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Env
  -> SourceSpan
  -> ModuleName
  -> Imports
  -> Exports
  -> [DeclarationRef]
  -> m Exports
resolveExports env ss mn imps exps refs =
  warnAndRethrow (addHint (ErrorInModule mn)) $ do
    filtered <- filterModule mn exps refs
    exps' <- foldM elaborateModuleExports filtered refs
    warnDuplicateRefs ss DuplicateExportRef refs
    return exps'

  where

  -- Takes the current module's imports, the accumulated list of exports, and a
  -- `DeclarationRef` for an explicit export. When the ref refers to another
  -- module, export anything from the imports that matches for that module.
  elaborateModuleExports :: Exports -> DeclarationRef -> m Exports
  elaborateModuleExports result (ModuleRef _ name) | name == mn = do
    let types' = exportedTypes result `M.union` exportedTypes exps
    let typeOps' = exportedTypeOps result `M.union` exportedTypeOps exps
    let classes' = exportedTypeClasses result `M.union` exportedTypeClasses exps
    let values' = exportedValues result `M.union` exportedValues exps
    let valueOps' = exportedValueOps result `M.union` exportedValueOps exps
    let kinds' = exportedKinds result `M.union` exportedKinds exps
    return result
      { exportedTypes = types'
      , exportedTypeOps = typeOps'
      , exportedTypeClasses = classes'
      , exportedValues = values'
      , exportedValueOps = valueOps'
      , exportedKinds = kinds'
      }
  elaborateModuleExports result (ModuleRef ss' name) = do
    let isPseudo = isPseudoModule name
    when (not isPseudo && not (isImportedModule name))
      . throwError . errorMessage' ss' . UnknownExport $ ModName name
    reTypes <- extract isPseudo name TyName (importedTypes imps)
    reTypeOps <- extract isPseudo name TyOpName (importedTypeOps imps)
    reDctors <- extract isPseudo name DctorName (importedDataConstructors imps)
    reClasses <- extract isPseudo name TyClassName (importedTypeClasses imps)
    reValues <- extract isPseudo name IdentName (importedValues imps)
    reValueOps <- extract isPseudo name ValOpName (importedValueOps imps)
    reKinds <- extract isPseudo name KiName (importedKinds imps)
    foldM (\exps' ((tctor, dctors), mn') -> exportType ReExport exps' tctor dctors mn') result (resolveTypeExports reTypes reDctors)
      >>= flip (foldM (uncurry . exportTypeOp)) (map resolveTypeOp reTypeOps)
      >>= flip (foldM (uncurry . exportTypeClass ReExport)) (map resolveClass reClasses)
      >>= flip (foldM (uncurry . exportValue)) (map resolveValue reValues)
      >>= flip (foldM (uncurry . exportValueOp)) (map resolveValueOp reValueOps)
      >>= flip (foldM (uncurry . exportKind)) (map resolveKind reKinds)
  elaborateModuleExports result _ = return result

  -- Extracts a list of values for a module based on a lookup table. If the
  -- boolean is true the values are filtered by the qualification
  extract
    :: Bool
    -> ModuleName
    -> (a -> Name)
    -> M.Map (Qualified a) [ImportRecord a]
    -> m [Qualified a]
  extract useQual name toName = fmap (map (importName . head . snd)) . go . M.toList
    where
    go = filterM $ \(name', options) -> do
      let isMatch = if useQual then isQualifiedWith name name' else any (checkUnqual name') options
      when (isMatch && length options > 1) $ void $ checkImportConflicts mn toName options
      return isMatch
    checkUnqual name' ir = isUnqualified name' && isQualifiedWith name (importName ir)

  -- Check whether a module name refers to a "pseudo module" that came into
  -- existence in an import scope due to importing one or more modules as
  -- qualified.
  isPseudoModule :: ModuleName -> Bool
  isPseudoModule = testQuals M.keys
    where
    -- Test for the presence of a `ModuleName` in a set of imports, using a
    -- function to either extract the keys or values. We test the keys to see if a
    -- value being re-exported belongs to a qualified module, and we test the
    -- values if that fails to see whether the value has been imported at all.
    testQuals :: (forall a b. M.Map (Qualified a) b -> [Qualified a]) -> ModuleName -> Bool
    testQuals f mn' = any (isQualifiedWith mn') (f (importedTypes imps))
                   || any (isQualifiedWith mn') (f (importedDataConstructors imps))
                   || any (isQualifiedWith mn') (f (importedTypeClasses imps))
                   || any (isQualifiedWith mn') (f (importedValues imps))
                   || any (isQualifiedWith mn') (f (importedValueOps imps))
                   || any (isQualifiedWith mn') (f (importedKinds imps))

  -- Check whether a module name refers to a module that has been imported
  -- without qualification into an import scope.
  isImportedModule :: ModuleName -> Bool
  isImportedModule = flip elem (importedModules imps)

  -- Constructs a list of types with their data constructors and the original
  -- module they were defined in from a list of type and data constructor names.
  resolveTypeExports
    :: [Qualified (ProperName 'TypeName)]
    -> [Qualified (ProperName 'ConstructorName)]
    -> [((ProperName 'TypeName, [ProperName 'ConstructorName]), ModuleName)]
  resolveTypeExports tctors dctors = map go tctors
    where
    go
      :: Qualified (ProperName 'TypeName)
      -> ((ProperName 'TypeName, [ProperName 'ConstructorName]), ModuleName)
    go (Qualified (Just mn'') name) =
      fromMaybe (internalError "Missing value in resolveTypeExports") $ do
        exps' <- envModuleExports <$> mn'' `M.lookup` env
        (dctors', mnOrig) <- name `M.lookup` exportedTypes exps'
        let relevantDctors = mapMaybe (disqualifyFor (Just mn'')) dctors
        return ((name, relevantDctors `intersect` dctors'), mnOrig)
    go (Qualified Nothing _) = internalError "Unqualified value in resolveTypeExports"

  -- Looks up an imported type operator and re-qualifies it with the original
  -- module it came from.
  resolveTypeOp :: Qualified (OpName 'TypeOpName) -> (OpName 'TypeOpName, ModuleName)
  resolveTypeOp op
    = splitQual
    . fromMaybe (internalError "Missing value in resolveValue")
    $ resolve exportedTypeOps op

  -- Looks up an imported class and re-qualifies it with the original module it
  -- came from.
  resolveClass :: Qualified (ProperName 'ClassName) -> (ProperName 'ClassName, ModuleName)
  resolveClass className
    = splitQual
    . fromMaybe (internalError "Missing value in resolveClass")
    $ resolve exportedTypeClasses className

  -- Looks up an imported value and re-qualifies it with the original module it
  -- came from.
  resolveValue :: Qualified Ident -> (Ident, ModuleName)
  resolveValue ident
    = splitQual
    . fromMaybe (internalError "Missing value in resolveValue")
    $ resolve exportedValues ident

  -- Looks up an imported operator and re-qualifies it with the original
  -- module it came from.
  resolveValueOp :: Qualified (OpName 'ValueOpName) -> (OpName 'ValueOpName, ModuleName)
  resolveValueOp op
    = splitQual
    . fromMaybe (internalError "Missing value in resolveValueOp")
    $ resolve exportedValueOps op

  -- Looks up an imported kind and re-qualifies it with the original
  -- module it came from.
  resolveKind :: Qualified (ProperName 'KindName) -> (ProperName 'KindName, ModuleName)
  resolveKind kind
    = splitQual
    . fromMaybe (internalError "Missing value in resolveKind")
    $ resolve exportedKinds kind

  resolve
    :: Ord a
    => (Exports -> M.Map a ModuleName)
    -> Qualified a
    -> Maybe (Qualified a)
  resolve f (Qualified (Just mn'') a) = do
    exps' <- envModuleExports <$> mn'' `M.lookup` env
    mn''' <- a `M.lookup` f exps'
    return $ Qualified (Just mn''') a
  resolve _ _ = internalError "Unqualified value in resolve"

  -- A partial function that takes a qualified value and extracts the value and
  -- qualified module components.
  splitQual :: Qualified a -> (a, ModuleName)
  splitQual (Qualified (Just mn'') a) = (a, mn'')
  splitQual _ = internalError "Unqualified value in splitQual"

-- |
-- Filters the full list of exportable values, types, and classes for a module
-- based on a list of export declaration references.
--
filterModule
  :: forall m
   . MonadError MultipleErrors m
  => ModuleName
  -> Exports
  -> [DeclarationRef]
  -> m Exports
filterModule mn exps refs = do
  types <- foldM filterTypes M.empty (combineTypeRefs refs)
  typeOps <- foldM (filterExport TyOpName getTypeOpRef exportedTypeOps) M.empty refs
  classes <- foldM (filterExport TyClassName getTypeClassRef exportedTypeClasses) M.empty refs
  values <- foldM (filterExport IdentName getValueRef exportedValues) M.empty refs
  valueOps <- foldM (filterExport ValOpName getValueOpRef exportedValueOps) M.empty refs
  kinds <- foldM (filterExport KiName getKindRef exportedKinds) M.empty refs
  return Exports
    { exportedTypes = types
    , exportedTypeOps = typeOps
    , exportedTypeClasses = classes
    , exportedValues = values
    , exportedValueOps = valueOps
    , exportedKinds = kinds
    }

  where

  -- Takes the list of exported refs, filters out any non-TypeRefs, then
  -- combines any duplicate type exports to ensure that all constructors
  -- listed for the type are covered. Without this, only the data constructor
  -- listing for the last ref would be used.
  combineTypeRefs :: [DeclarationRef] -> [DeclarationRef]
  combineTypeRefs
    = fmap (\(ss', (tc, dcs)) -> TypeRef ss' tc dcs)
    . fmap (foldr1 $ \(ss, (tc, dcs1)) (_, (_, dcs2)) -> (ss, (tc, liftM2 (++) dcs1 dcs2)))
    . groupBy ((==) `on` (fst . snd))
    . sortBy (compare `on` (fst . snd))
    . mapMaybe (\ref -> (declRefSourceSpan ref,) <$> getTypeRef ref)

  filterTypes
    :: M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ModuleName)
    -> DeclarationRef
    -> m (M.Map (ProperName 'TypeName) ([ProperName 'ConstructorName], ModuleName))
  filterTypes result (TypeRef ss name expDcons) =
    case name `M.lookup` exportedTypes exps of
      Nothing -> throwError . errorMessage' ss . UnknownExport $ TyName name
      Just (dcons, _) -> do
        let expDcons' = fromMaybe dcons expDcons
        traverse_ (checkDcon name dcons) expDcons'
        return $ M.insert name (expDcons', mn) result
    where
    -- Ensures a data constructor is exportable for a given type. Takes a type
    -- name, a list of exportable data constructors for the type, and the name of
    -- the data constructor to check.
    checkDcon
      :: ProperName 'TypeName
      -> [ProperName 'ConstructorName]
      -> ProperName 'ConstructorName
      -> m ()
    checkDcon tcon dcons dcon =
      unless (dcon `elem` dcons) .
        throwError . errorMessage' ss $ UnknownExportDataConstructor tcon dcon
  filterTypes result _ = return result

  filterExport
    :: Ord a
    => (a -> Name)
    -> (DeclarationRef -> Maybe a)
    -> (Exports -> M.Map a ModuleName)
    -> M.Map a ModuleName
    -> DeclarationRef
    -> m (M.Map a ModuleName)
  filterExport toName get fromExps result ref
    | Just name <- get ref =
        case name `M.lookup` fromExps exps of
          -- TODO: I'm not sure if we actually need to check mn == mn' here -gb
          Just mn' | mn == mn' -> return $ M.insert name mn result
          _ -> throwError . errorMessage' (declRefSourceSpan ref) . UnknownExport $ toName name
  filterExport _ _ _ result _ = return result
