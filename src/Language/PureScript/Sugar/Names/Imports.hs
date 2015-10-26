-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Names.Imports
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.PureScript.Sugar.Names.Imports
  ( resolveImports
  , resolveModuleImport
  ) where

import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)

import Control.Arrow (first)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..), (<$>))
#endif
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer (MonadWriter(..), censor)

import qualified Data.Map as M

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Errors
import Language.PureScript.Sugar.Names.Env

-- Finds the imports within a module, mapping the imported module name to an optional set of
-- explicitly imported declarations.
findImports :: forall m. (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) => [Declaration] -> m (M.Map ModuleName [(Maybe SourceSpan, ImportDeclarationType, Maybe ModuleName)])
findImports = foldM (go Nothing) M.empty
  where
  go pos result (ImportDeclaration mn typ qual) = do
    checkImportRefType typ
    let imp = (pos, typ, qual)
    return $ M.insert mn (maybe [imp] (imp :) (mn `M.lookup` result)) result
  go _ result (PositionedDeclaration pos _ d) = rethrowWithPosition pos $ go (Just pos) result d
  go _ result _ = return result

  -- Ensure that classes don't appear in an `import X hiding (...)`
  checkImportRefType :: ImportDeclarationType -> m ()
  checkImportRefType (Hiding refs) = mapM_ checkImportRef refs
  checkImportRefType _ = return ()
  checkImportRef :: DeclarationRef -> m ()
  checkImportRef (ModuleRef name) = throwError . errorMessage $ ImportHidingModule name
  checkImportRef _ = return ()

-- |
-- Constructs a set of imports for a module.
--
resolveImports :: (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) => Env -> Module -> m Imports
resolveImports env (Module _ _ currentModule decls _) =
  censor (addHint (ErrorInModule currentModule)) $ do
    scope <- M.insert currentModule [(Nothing, Implicit, Nothing)] <$> findImports decls
    foldM (resolveModuleImport currentModule env) nullImports (M.toList scope)

-- | Constructs a set of imports for a single module import.
resolveModuleImport ::
  forall m. (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) =>
  ModuleName -> Env -> Imports ->
  (ModuleName, [(Maybe SourceSpan, ImportDeclarationType, Maybe ModuleName)]) ->
  m Imports
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
resolveImport :: forall m. (Applicative m, MonadError MultipleErrors m, MonadWriter MultipleErrors m) => ModuleName -> ModuleName -> Exports -> Imports -> Maybe ModuleName -> ImportDeclarationType -> m Imports
resolveImport currentModule importModule exps imps impQual =
  resolveByType
  where

  resolveByType :: ImportDeclarationType -> m Imports
  resolveByType Implicit = importAll importExplicit
  resolveByType (Explicit explImports) = checkRefs explImports >> foldM importExplicit imps explImports
  resolveByType (Hiding hiddenImports) = checkRefs hiddenImports >> importAll (importNonHidden hiddenImports)

  -- Check that a 'DeclarationRef' refers to an importable symbol
  checkRefs :: [DeclarationRef] -> m ()
  checkRefs = mapM_ check
    where
    check (PositionedDeclarationRef pos _ r) =
      rethrowWithPosition pos $ check r
    check (ValueRef name) =
      checkImportExists UnknownImportValue (fst `map` exportedValues exps) name
    check (TypeRef name dctors) = do
      checkImportExists UnknownImportType ((fst . fst) `map` exportedTypes exps) name
      let allDctors = fst `map` allExportedDataConstructors name
      maybe (return ()) (mapM_ $ checkDctorExists name allDctors) dctors
    check (TypeClassRef name) =
      checkImportExists UnknownImportTypeClass (fst `map` exportedTypeClasses exps) name
    --check (ModuleRef name) =
    --  checkImportExists (const UnknownModule) (exportedModules exps) name
    check _ = internalError "Invalid argument to checkRefs"

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
    rethrowWithPosition pos . warnWithPosition pos $ importExplicit imp r
  importExplicit imp (ValueRef name) = do
    values' <- updateImports (importedValues imp) showIdent (exportedValues exps) name
    return $ imp { importedValues = values' }
  importExplicit imp (TypeRef name dctors) = do
    types' <- updateImports (importedTypes imp) runProperName (first fst `map` exportedTypes exps) name
    let exportedDctors :: [(ProperName, ModuleName)]
        exportedDctors = allExportedDataConstructors name
        dctorNames :: [ProperName]
        dctorNames = fst `map` exportedDctors
    maybe (return ()) (mapM_ $ checkDctorExists name dctorNames) dctors
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
  updateImports :: (Ord a) => M.Map (Qualified a) (Qualified a, ModuleName)
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
