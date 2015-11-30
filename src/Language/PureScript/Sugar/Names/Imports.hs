{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Language.PureScript.Sugar.Names.Imports
  ( resolveImports
  , resolveModuleImport
  , findImports
  ) where

import Prelude ()
import Prelude.Compat

import Data.List (find, delete, (\\))
import Data.Maybe (fromMaybe, isJust, isNothing, fromJust)
import Data.Foldable (traverse_, for_)
import Data.Traversable (for)

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer (MonadWriter(..))

import qualified Data.Map as M

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Errors
import Language.PureScript.Sugar.Names.Env

-- |
-- Finds the imports within a module, mapping the imported module name to an optional set of
-- explicitly imported declarations.
--
findImports
  :: forall m
   . (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => [Declaration]
  -> m (M.Map ModuleName [(Maybe SourceSpan, ImportDeclarationType, Maybe ModuleName)])
findImports = foldM (go Nothing) M.empty
  where
  go pos result (ImportDeclaration mn typ qual isOldSyntax) = do
    when isOldSyntax . tell . errorMessage $ DeprecatedQualifiedSyntax mn (fromJust qual)
    let imp = (pos, typ, qual)
    return $ M.insert mn (maybe [imp] (imp :) (mn `M.lookup` result)) result
  go _ result (PositionedDeclaration pos _ d) = warnAndRethrowWithPosition pos $ go (Just pos) result d
  go _ result _ = return result

type ImportDef = (Maybe SourceSpan, ImportDeclarationType, Maybe ModuleName)

-- |
-- Constructs a set of imports for a module.
--
resolveImports
  :: forall m
   . (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => Env
  -> Module
  -> m (Module, Imports)
resolveImports env (Module ss coms currentModule decls exps) =
  warnAndRethrow (addHint (ErrorInModule currentModule)) $ do

    decls' <- traverse updateImportRef decls
    imports <- findImports decls'

    for_ (M.toList imports) $ \(mn, imps) -> do

      -- Better ordering for the warnings: the list is in last-import-first
      -- order, but we want the first appearence of an import to be the primary,
      -- and warnings to appear for later imports
      let imps' = reverse imps

      warned <- foldM (checkDuplicateImports mn) [] (selfCartesianSubset imps')

      let unqual = filter (\(_, _, q) -> isJust q) (imps' \\ warned)

      warned' <- (warned ++) <$>
        if (length unqual < 2)
        then return []
        else case find (\(_, typ, _) -> typ == Implicit) unqual of
          Just i ->
            for (delete i unqual) $ \i'@(pos, typ, _) -> do
              warn pos $ RedundantUnqualifiedImport mn typ
              return i'
          Nothing ->
            for (tail unqual) $ \i@(pos, _, _) -> do
              warn pos $ DuplicateSelectiveImport mn
              return i

      for_ (imps' \\ warned') $ \(pos, typ, _) ->
        let (dupeRefs, dupeDctors) = findDuplicateRefs $ case typ of
              Explicit refs -> refs
              Hiding refs -> refs
              _ -> []
        in warnDupeRefs pos dupeRefs >> warnDupeDctors pos dupeDctors

      return ()

    let scope = M.insert currentModule [(Nothing, Implicit, Nothing)] imports
    resolved <- foldM (resolveModuleImport currentModule env) nullImports (M.toList scope)
    return (Module ss coms currentModule decls' exps, resolved)

  where
  selfCartesianSubset :: [a] -> [(a, a)]
  selfCartesianSubset (x : xs) = [(x, y) | y <- xs] ++ selfCartesianSubset xs
  selfCartesianSubset [] = []

  checkDuplicateImports :: ModuleName -> [ImportDef] -> (ImportDef, ImportDef) -> m [ImportDef]
  checkDuplicateImports mn xs ((_, t1, q1), (pos, t2, q2)) =
    if (t1 == t2 && q1 == q2)
    then do
      warn pos $ DuplicateImport mn t2 q2
      return $ (pos, t2, q2) : xs
    else return xs

  warnDupeRefs :: Maybe SourceSpan -> [DeclarationRef] -> m ()
  warnDupeRefs pos = traverse_ $ \case
    TypeRef name _ -> warnDupe pos $ "type " ++ runProperName name
    ValueRef name -> warnDupe pos $ "value " ++ runIdent name
    TypeClassRef name -> warnDupe pos $ "class " ++ runProperName name
    ModuleRef name -> warnDupe pos $ "module " ++ runModuleName name
    _ -> return ()

  warnDupeDctors :: Maybe SourceSpan -> [ProperName] -> m ()
  warnDupeDctors pos = traverse_ (warnDupe pos . ("data constructor " ++) . runProperName)

  warnDupe :: Maybe SourceSpan -> String -> m ()
  warnDupe pos ref = warn pos $ DuplicateImportRef ref

  warn :: Maybe SourceSpan -> SimpleErrorMessage -> m ()
  warn pos msg = maybe id warnWithPosition pos $ tell . errorMessage $ msg

  updateImportRef :: Declaration -> m Declaration
  updateImportRef (PositionedDeclaration pos com d) =
    warnWithPosition pos $ PositionedDeclaration pos com <$> updateImportRef d
  updateImportRef (ImportDeclaration mn typ qual isOldSyntax) = do
    modExports <- getExports env mn
    typ' <- case typ of
      Implicit -> return Implicit
      Explicit refs -> Explicit <$> updateProperRef mn modExports `traverse` refs
      Hiding refs -> Hiding <$> updateProperRef mn modExports `traverse` refs
    return $ ImportDeclaration mn typ' qual isOldSyntax
  updateImportRef other = return other

  updateProperRef :: ModuleName -> Exports -> DeclarationRef -> m DeclarationRef
  updateProperRef importModule modExports (ProperRef name) =
    if name `elem` (fst `map` exportedTypeClasses modExports)
    then do
      tell . errorMessage $ DeprecatedClassImport importModule name
      return $ TypeClassRef name
    else return $ TypeRef name (Just [])
  updateProperRef importModule modExports (PositionedDeclarationRef pos com ref) =
    PositionedDeclarationRef pos com <$> updateProperRef importModule modExports ref
  updateProperRef _ _ other = return other

-- | Constructs a set of imports for a single module import.
resolveModuleImport
  :: forall m
   . (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ModuleName
  -> Env
  -> Imports
  -> (ModuleName, [(Maybe SourceSpan, ImportDeclarationType, Maybe ModuleName)])
  -> m Imports
resolveModuleImport currentModule env ie (mn, imps) = foldM go ie imps
  where
  go :: Imports -> (Maybe SourceSpan, ImportDeclarationType, Maybe ModuleName) -> m Imports
  go ie' (pos, typ, impQual) = do
    modExports <- positioned $ maybe (throwError . errorMessage $ UnknownModule mn) (return . envModuleExports) $ mn `M.lookup` env
    let ie'' = ie' { importedModules = mn : importedModules ie' }
    positioned $ resolveImport currentModule mn modExports ie'' impQual typ
    where
    positioned err = case pos of
      Nothing -> err
      Just pos' -> rethrowWithPosition pos' err

-- |
-- Extends the local environment for a module by resolving an import of another module.
--
resolveImport
  :: forall m
   . (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
  => ModuleName
  -> ModuleName
  -> Exports
  -> Imports
  -> Maybe ModuleName
  -> ImportDeclarationType
  -> m Imports
resolveImport currentModule importModule exps imps impQual = resolveByType
  where

  resolveByType :: ImportDeclarationType -> m Imports
  resolveByType Implicit = importAll importExplicit
  resolveByType (Explicit refs) = checkRefs False refs >> foldM importExplicit imps refs
  resolveByType (Hiding refs) = do
    imps' <- checkRefs True refs >> importAll (importNonHidden refs)
    let isEmptyImport
           = M.null (importedTypes imps')
          && M.null (importedDataConstructors imps')
          && M.null (importedTypeClasses imps')
          && M.null (importedValues imps')
    when isEmptyImport $ tell . errorMessage $ RedundantEmptyHidingImport importModule
    return imps'

  -- Check that a 'DeclarationRef' refers to an importable symbol
  checkRefs :: Bool -> [DeclarationRef] -> m ()
  checkRefs isHiding = traverse_ check
    where
    check (PositionedDeclarationRef pos _ r) =
      rethrowWithPosition pos $ check r
    check (ValueRef name) =
      checkImportExists UnknownImportValue (fst `map` exportedValues exps) name
    check (TypeRef name dctors) = do
      checkImportExists UnknownImportType ((fst . fst) `map` exportedTypes exps) name
      let allDctors = fst `map` allExportedDataConstructors name
      maybe (return ()) (traverse_ $ checkDctorExists name allDctors) dctors
    check (TypeClassRef name) =
      checkImportExists UnknownImportTypeClass (fst `map` exportedTypeClasses exps) name
    check (ModuleRef name) | isHiding =
      throwError . errorMessage $ ImportHidingModule name
    check r = internalError $ "Invalid argument to checkRefs: " ++ show r

  -- Check that an explicitly imported item exists in the module it is being imported from
  checkImportExists :: (Eq a) => (ModuleName -> a -> SimpleErrorMessage) -> [a] -> a -> m ()
  checkImportExists unknown exports item =
    when (item `notElem` exports) $ throwError . errorMessage $ unknown importModule item

  -- Ensure that an explicitly imported data constructor exists for the type it is being imported
  -- from
  checkDctorExists :: ProperName -> [ProperName] -> ProperName -> m ()
  checkDctorExists tcon = checkImportExists (flip UnknownImportDataConstructor tcon)

  importNonHidden :: [DeclarationRef] -> Imports -> DeclarationRef -> m Imports
  importNonHidden hidden m ref | isHidden ref = return m
                               | otherwise = importExplicit m ref
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
  importAll importer = do
    imp' <- foldM (\m ((name, dctors), _) -> importer m (TypeRef name (Just dctors))) imps (exportedTypes exps)
    imp'' <- foldM (\m (name, _) -> importer m (ValueRef name)) imp' (exportedValues exps)
    foldM (\m (name, _) -> importer m (TypeClassRef name)) imp'' (exportedTypeClasses exps)

  -- Import something explicitly
  importExplicit :: Imports -> DeclarationRef -> m Imports
  importExplicit imp (PositionedDeclarationRef pos _ r) =
    warnAndRethrowWithPosition pos $ importExplicit imp r
  importExplicit imp (ValueRef name) = do
    values' <- updateImports (importedValues imp) showIdent (exportedValues exps) name
    return $ imp { importedValues = values' }
  importExplicit imp (TypeRef name dctors) = do
    types' <- updateImports (importedTypes imp) runProperName (first fst `map` exportedTypes exps) name
    let exportedDctors :: [(ProperName, ModuleName)]
        exportedDctors = allExportedDataConstructors name
        dctorNames :: [ProperName]
        dctorNames = fst `map` exportedDctors
    maybe (return ()) (traverse_ $ checkDctorExists name dctorNames) dctors
    when (null dctorNames && isNothing dctors) . tell . errorMessage $ MisleadingEmptyTypeImport importModule name
    dctors' <- foldM (\m -> updateImports m runProperName exportedDctors) (importedDataConstructors imp) (fromMaybe dctorNames dctors)
    return $ imp { importedTypes = types', importedDataConstructors = dctors' }
  importExplicit imp (TypeClassRef name) = do
    typeClasses' <- updateImports (importedTypeClasses imp) runProperName (exportedTypeClasses exps) name
    return $ imp { importedTypeClasses = typeClasses' }
  importExplicit _ _ = internalError "Invalid argument to importExplicit"

  -- Find all exported data constructors for a given type
  allExportedDataConstructors :: ProperName -> [(ProperName, ModuleName)]
  allExportedDataConstructors name =
    case find ((== name) . fst . fst) (exportedTypes exps) of
      Nothing -> internalError "Invalid state in allExportedDataConstructors"
      Just ((_, dctors), mn) -> map (, mn) dctors

  -- Add something to the Imports if it does not already exist there
  updateImports
    :: (Ord a)
    => M.Map (Qualified a) (Qualified a, ModuleName)
    -> (a -> String)
    -> [(a, ModuleName)]
    -> a
    -> m (M.Map (Qualified a) (Qualified a, ModuleName))
  updateImports imps' render exps' name = case M.lookup (Qualified impQual name) imps' of

    -- If the name is not already present add it to the list, after looking up
    -- where it was originally defined
    Nothing ->
      let mnOrig = fromMaybe (internalError "Invalid state in updateImports") (name `lookup` exps')
      in return $ M.insert (Qualified impQual name) (Qualified (Just importModule) name, mnOrig) imps'

    -- If the name already is present check whether it's a duplicate import
    -- before rejecting it. For example, if module A defines X, and module B
    -- re-exports A, importing A and B in C should not result in a "conflicting
    -- import for `x`" error
    Just (Qualified (Just mn) _, mnOrig)
       | mnOrig == fromMaybe (internalError "Invalid state in updateImports") (name `lookup` exps') -> return imps'
       | otherwise -> throwError . errorMessage $ err
        where
        err = if currentModule `elem` [mn, importModule]
              then ConflictingImport (render name) importModule
              else ConflictingImports (render name) mn importModule

    Just (Qualified Nothing _, _) ->
      internalError "Invalid state in updateImports"
