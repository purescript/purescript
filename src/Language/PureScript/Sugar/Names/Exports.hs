module Language.PureScript.Sugar.Names.Exports
  ( findExportable
  , resolveExports
  ) where

import Prelude.Compat

import Control.Monad
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Error.Class (MonadError(..))

import Data.Foldable (traverse_)
import Data.List (find, intersect)
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
  updateExports exps (ValueFixityDeclaration _ _ op) = exportValueOp exps op mn
  updateExports exps (TypeFixityDeclaration _ _ op) = exportTypeOp exps op mn
  updateExports exps (ExternDeclaration name _) = exportValue exps name mn
  updateExports exps (PositionedDeclaration pos _ d) = rethrowWithPosition pos $ updateExports exps d
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
  elaborateModuleExports result (PositionedDeclarationRef pos _ r) =
    warnAndRethrowWithPosition pos $ elaborateModuleExports result r
  elaborateModuleExports result (ModuleRef name) | name == mn = do
    let types' = exportedTypes result ++ exportedTypes exps
    let typeOps' = exportedTypeOps result ++ exportedTypeOps exps
    let classes' = exportedTypeClasses result ++ exportedTypeClasses exps
    let values' = exportedValues result ++ exportedValues exps
    let valueOps' = exportedValueOps result ++ exportedValueOps exps
    return result
      { exportedTypes = types'
      , exportedTypeOps = typeOps'
      , exportedTypeClasses = classes'
      , exportedValues = values'
      , exportedValueOps = valueOps'
      }
  elaborateModuleExports result (ModuleRef name) = do
    let isPseudo = isPseudoModule name
    when (not isPseudo && not (isImportedModule name))
      . throwError . errorMessage . UnknownExport $ ModName name
    reTypes <- extract isPseudo name TyName (importedTypes imps)
    reTypeOps <- extract isPseudo name TyOpName (importedTypeOps imps)
    reDctors <- extract isPseudo name DctorName (importedDataConstructors imps)
    reClasses <- extract isPseudo name TyClassName (importedTypeClasses imps)
    reValues <- extract isPseudo name IdentName (importedValues imps)
    reValueOps <- extract isPseudo name ValOpName (importedValueOps imps)
    foldM (\exps' ((tctor, dctors), mn') -> exportType exps' tctor dctors mn') result (resolveTypeExports reTypes reDctors)
      >>= flip (foldM (uncurry . exportTypeOp)) (map resolveTypeOp reTypeOps)
      >>= flip (foldM (uncurry . exportTypeClass)) (map resolveClass reClasses)
      >>= flip (foldM (uncurry . exportValue)) (map resolveValue reValues)
      >>= flip (foldM (uncurry . exportValueOp)) (map resolveValueOp reValueOps)
  elaborateModuleExports result _ = return result

  -- Extracts a list of values for a module based on a lookup table. If the
  -- boolean is true the values are filtered by the qualification
  extract
    :: (Show a, Ord a)
    => Bool
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
    go (Qualified (Just mn'') name) = fromMaybe (internalError "Missing value in resolveTypeExports") $ do
      exps' <- envModuleExports <$> mn'' `M.lookup` env
      ((_, dctors'), mnOrig) <- find (\((name', _), _) -> name == name') (exportedTypes exps')
      let relevantDctors = mapMaybe (\(Qualified mn''' dctor) -> if mn''' == Just mn'' then Just dctor else Nothing) dctors
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
   . MonadError MultipleErrors m
  => ModuleName
  -> Exports
  -> [DeclarationRef]
  -> m Exports
filterModule mn exps refs = do
  types <- foldM filterTypes [] refs
  typeOps <- foldM (filterExport TyOpName getTypeOpRef exportedTypeOps) [] refs
  classes <- foldM (filterExport TyClassName getTypeClassRef exportedTypeClasses) [] refs
  values <- foldM (filterExport IdentName getValueRef exportedValues) [] refs
  valueOps <- foldM (filterExport ValOpName getValueOpRef exportedValueOps) [] refs
  return Exports
    { exportedTypes = types
    , exportedTypeOps = typeOps
    , exportedTypeClasses = classes
    , exportedValues = values
    , exportedValueOps = valueOps
    }

  where

  filterTypes
    :: [((ProperName 'TypeName, [ProperName 'ConstructorName]), ModuleName)]
    -> DeclarationRef
    -> m [((ProperName 'TypeName, [ProperName 'ConstructorName]), ModuleName)]
  filterTypes result (PositionedDeclarationRef pos _ r) =
    rethrowWithPosition pos $ filterTypes result r
  filterTypes result (TypeRef name expDcons) =
    case matchType `find` exportedTypes exps of
      Nothing -> throwError . errorMessage . UnknownExport $ TyName name
      Just ((_, dcons), _) -> do
        let expDcons' = fromMaybe dcons expDcons
        traverse_ (checkDcon name dcons) expDcons'
        return $ ((name, expDcons'), mn) : result
    where
    -- Finds a type declaration by matching its name and defining module
    matchType ((name', _), mn') = name == name' && mn == mn'
    -- Ensures a data constructor is exportable for a given type. Takes a type
    -- name, a list of exportable data constructors for the type, and the name of
    -- the data constructor to check.
    checkDcon
      :: ProperName 'TypeName
      -> [ProperName 'ConstructorName]
      -> ProperName 'ConstructorName
      -> m ()
    checkDcon tcon dcons dcon =
      unless (dcon `elem` dcons) $
        throwError . errorMessage $ UnknownExportDataConstructor tcon dcon
  filterTypes result _ = return result

  filterExport
    :: Eq a
    => (a -> Name)
    -> (DeclarationRef -> Maybe a)
    -> (Exports -> [(a, ModuleName)])
    -> [(a, ModuleName)]
    -> DeclarationRef
    -> m [(a, ModuleName)]
  filterExport toName get fromExps result (PositionedDeclarationRef pos _ r) =
    rethrowWithPosition pos $ filterExport toName get fromExps result r
  filterExport toName get fromExps result ref
    | Just name <- get ref =
        if (name, mn) `elem` fromExps exps
        then return $ (name, mn) : result
        else throwError . errorMessage . UnknownExport $ toName name
  filterExport _ _ _ result _ = return result
