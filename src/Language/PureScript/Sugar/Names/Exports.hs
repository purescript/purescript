{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Language.PureScript.Sugar.Names.Exports
  ( findExportable
  , resolveExports
  ) where

import Prelude ()
import Prelude.Compat

import Data.List (find, intersect)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Foldable (traverse_)

import Control.Monad
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Error.Class (MonadError(..))

import qualified Data.Map as M

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Errors
import Language.PureScript.Sugar.Names.Env

-- |
-- Finds all exportable members of a module, disregarding any explicit exports.
--
findExportable :: forall m. (MonadError MultipleErrors m) => Module -> m Exports
findExportable (Module _ _ mn ds _) =
  rethrow (addHint (ErrorInModule mn)) $ foldM updateExports nullExports ds
  where
  updateExports :: Exports -> Declaration -> m Exports
  updateExports exps (TypeClassDeclaration tcn _ _ ds') = do
    exps' <- exportTypeClass exps tcn mn
    foldM go exps' ds'
    where
    go exps'' (TypeDeclaration name _) = exportValue exps'' name mn
    go exps'' (PositionedDeclaration pos _ d) = rethrowWithPosition pos $ go exps'' d
    go _ _ = internalError "Invalid declaration in TypeClassDeclaration"
  updateExports exps (DataDeclaration _ tn _ dcs) = exportType exps tn (map fst dcs) mn
  updateExports exps (TypeSynonymDeclaration tn _ _) = exportType exps tn [] mn
  updateExports exps (ExternDataDeclaration tn _) = exportType exps tn [] mn
  updateExports exps (ValueDeclaration name _ _ _) = exportValue exps name mn
  updateExports exps (FixityDeclaration _ name (Just _)) = exportValue exps (Op name) mn
  updateExports exps (ExternDeclaration name _) = exportValue exps name mn
  updateExports exps (PositionedDeclaration pos _ d) = rethrowWithPosition pos $ updateExports exps d
  updateExports exps _ = return exps

-- |
-- Resolves the exports for a module, filtering out members that have not been
-- exported and elaborating re-exports of other modules.
--
resolveExports :: forall m. (MonadError MultipleErrors m, MonadWriter MultipleErrors m) => Env -> ModuleName -> Imports -> Exports -> [DeclarationRef] -> m Exports
resolveExports env mn imps exps refs =
  rethrow (addHint (ErrorInModule mn)) $ do
    filtered <- filterModule mn exps refs
    let (dupeRefs, dupeDctors) = findDuplicateRefs refs
    warnDupeRefs dupeRefs
    warnDupeDctors dupeDctors
    foldM elaborateModuleExports filtered refs

  where

  warnDupeRefs :: [DeclarationRef] -> m ()
  warnDupeRefs = traverse_ $ \case
    TypeRef name _ -> warnDupe $ "type " ++ runProperName name
    ValueRef name -> warnDupe $ "value " ++ runIdent name
    TypeClassRef name -> warnDupe $ "class " ++ runProperName name
    ModuleRef name -> warnDupe $ "module " ++ runModuleName name
    _ -> return ()

  warnDupeDctors :: [ProperName 'ConstructorName] -> m ()
  warnDupeDctors = traverse_ (warnDupe . ("data constructor " ++) . runProperName)

  warnDupe :: String -> m ()
  warnDupe ref = tell . errorMessage $ DuplicateExportRef ref

  -- Takes the current module's imports, the accumulated list of exports, and a
  -- `DeclarationRef` for an explicit export. When the ref refers to another
  -- module, export anything from the imports that matches for that module.
  elaborateModuleExports :: Exports -> DeclarationRef -> m Exports
  elaborateModuleExports result (PositionedDeclarationRef pos _ r) =
    rethrowWithPosition pos $ elaborateModuleExports result r
  elaborateModuleExports result (ModuleRef name) | name == mn = do
    let types' = exportedTypes result ++ exportedTypes exps
    let classes' = exportedTypeClasses result ++ exportedTypeClasses exps
    let values' = exportedValues result ++ exportedValues exps
    return result { exportedTypes = types'
                  , exportedTypeClasses = classes'
                  , exportedValues = values' }
  elaborateModuleExports result (ModuleRef name) = do
    let isPseudo = isPseudoModule name
    when (not isPseudo && not (isImportedModule name)) $
      throwError . errorMessage . UnknownExportModule $ name
    reTypes <- extract isPseudo name (("type " ++) . runProperName) (importedTypes imps)
    reDctors <- extract isPseudo name (("data constructor " ++) . runProperName) (importedDataConstructors imps)
    reClasses <- extract isPseudo name (("class " ++) . runProperName) (importedTypeClasses imps)
    reValues <- extract isPseudo name (("value " ++) . runIdent) (importedValues imps)
    result' <- foldM (\exps' ((tctor, dctors), mn') -> exportType exps' tctor dctors mn') result (resolveTypeExports reTypes reDctors)
    result'' <- foldM (uncurry . exportTypeClass) result' (map resolveClass reClasses)
    foldM (uncurry . exportValue) result'' (map resolveValue reValues)
  elaborateModuleExports result _ = return result

  -- Extracts a list of values for a module based on a lookup table. If the
  -- boolean is true the values are filtered by the qualification
  extract
    :: (Show a, Ord a)
    => Bool
    -> ModuleName
    -> (a -> String)
    -> M.Map (Qualified a) [ImportRecord a]
    -> m [Qualified a]
  extract useQual name render = fmap (map (importName . head . snd)) . go . M.toList
    where
    go = filterM $ \(name', options) -> do
      let isMatch = if useQual then eqQual name name' else any (eqQual name . importName) options
      when (isMatch && length options > 1) $ void $ checkImportConflicts mn render options
      return isMatch

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
    testQuals f mn' = any (eqQual mn') (f (importedTypes imps))
                   || any (eqQual mn') (f (importedDataConstructors imps))
                   || any (eqQual mn') (f (importedTypeClasses imps))
                   || any (eqQual mn') (f (importedValues imps))

  -- Check whether a module name refers to a module that has been imported
  -- without qualification into an import scope.
  isImportedModule :: ModuleName -> Bool
  isImportedModule = flip elem (importedModules imps)

  -- Check whether a module name matches that of a qualified value.
  eqQual :: ModuleName -> Qualified a -> Bool
  eqQual mn'' (Qualified (Just mn''') _) = mn'' == mn'''
  eqQual _ _ = False

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
    go (Qualified (Just mn'') name) = fromMaybe (internalError "Missing value in resolveTypeExports") $ do
      exps' <- envModuleExports <$> mn'' `M.lookup` env
      ((_, dctors'), mnOrig) <- find (\((name', _), _) -> name == name') (exportedTypes exps')
      let relevantDctors = mapMaybe (\(Qualified mn''' dctor) -> if mn''' == Just mn'' then Just dctor else Nothing) dctors
      return ((name, intersect relevantDctors dctors'), mnOrig)
    go (Qualified Nothing _) = internalError "Unqualified value in resolveTypeExports"


  -- Looks up an imported class and re-qualifies it with the original module it
  -- came from.
  resolveClass :: Qualified (ProperName 'ClassName) -> (ProperName 'ClassName, ModuleName)
  resolveClass className = splitQual $ fromMaybe (internalError "Missing value in resolveClass") $
    resolve exportedTypeClasses className

  -- Looks up an imported value and re-qualifies it with the original module it
  -- came from.
  resolveValue :: Qualified Ident -> (Ident, ModuleName)
  resolveValue ident = splitQual $ fromMaybe (internalError "Missing value in resolveValue") $
    resolve exportedValues ident

  resolve :: (Eq a) => (Exports -> [(a, ModuleName)]) -> Qualified a -> Maybe (Qualified a)
  resolve f (Qualified (Just mn'') a) = do
    exps' <- envModuleExports <$> mn'' `M.lookup` env
    mn''' <- snd <$> find ((== a) . fst) (f exps')
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
   . (MonadError MultipleErrors m)
  => ModuleName
  -> Exports
  -> [DeclarationRef]
  -> m Exports
filterModule mn exps refs = do
  types <- foldM (filterTypes $ exportedTypes exps) [] refs
  values <- foldM (filterValues $ exportedValues exps) [] refs
  classes <- foldM (filterClasses $ exportedTypeClasses exps) [] refs
  return exps { exportedTypes = types , exportedTypeClasses = classes , exportedValues = values }

  where

  -- Takes a list of all the exportable types with their data constructors, the
  -- accumulated list of filtered exports, and a `DeclarationRef` for an
  -- explicit export. When the ref refers to a type in the list of exportable
  -- values, the type and specified data constructors are included in the
  -- result.
  filterTypes
    :: [((ProperName 'TypeName, [ProperName 'ConstructorName]), ModuleName)]
    -> [((ProperName 'TypeName, [ProperName 'ConstructorName]), ModuleName)]
    -> DeclarationRef
    -> m [((ProperName 'TypeName, [ProperName 'ConstructorName]), ModuleName)]
  filterTypes exps' result (PositionedDeclarationRef pos _ r) =
    rethrowWithPosition pos $ filterTypes exps' result r
  filterTypes exps' result (TypeRef name expDcons) =
    case (\((name', _), mn') -> name == name' && mn == mn') `find` exps' of
      Nothing -> throwError . errorMessage . UnknownExportType $ name
      Just ((_, dcons), _) -> do
        let expDcons' = fromMaybe dcons expDcons
        traverse_ (checkDcon name dcons) expDcons'
        return $ ((name, expDcons'), mn) : result
  filterTypes _ result _ = return result

  -- Ensures a data constructor is exportable for a given type. Takes a type
  -- name, a list of exportable data constructors for the type, and the name of
  -- the data constructor to check.
  checkDcon
    :: ProperName 'TypeName
    -> [ProperName 'ConstructorName]
    -> ProperName 'ConstructorName
    -> m ()
  checkDcon tcon exps' name =
    unless (name `elem` exps') $
      throwError . errorMessage $ UnknownExportDataConstructor tcon name

  -- Takes a list of all the exportable classes, the accumulated list of
  -- filtered exports, and a `DeclarationRef` for an explicit export. When the
  -- ref refers to a class in the list of exportable classes, the class is
  -- included in the result.
  filterClasses
    :: [(ProperName 'ClassName, ModuleName)]
    -> [(ProperName 'ClassName, ModuleName)]
    -> DeclarationRef
    -> m [(ProperName 'ClassName, ModuleName)]
  filterClasses exps' result (PositionedDeclarationRef pos _ r) =
    rethrowWithPosition pos $ filterClasses exps' result r
  filterClasses exps' result (TypeClassRef name) =
    if (name, mn) `elem` exps'
    then return $ (name, mn) : result
    else throwError . errorMessage . UnknownExportTypeClass $ name
  filterClasses _ result _ = return result

  -- Takes a list of all the exportable values, the accumulated list of filtered
  -- exports, and a `DeclarationRef` for an explicit export. When the ref refers
  -- to a value in the list of exportable values, the value is included in the
  -- result.
  filterValues :: [(Ident, ModuleName)] -> [(Ident, ModuleName)] -> DeclarationRef -> m [(Ident, ModuleName)]
  filterValues exps' result (PositionedDeclarationRef pos _ r) =
    rethrowWithPosition pos $ filterValues exps' result r
  filterValues exps' result (ValueRef name) =
    if (name, mn) `elem` exps'
    then return $ (name, mn) : result
    else throwError . errorMessage . UnknownExportValue $ name
  filterValues _ result _ = return result
