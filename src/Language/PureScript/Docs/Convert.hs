-- | Functions for converting PureScript ASTs into values of the data types
-- from Language.PureScript.Docs.

module Language.PureScript.Docs.Convert
  ( convertModules
  , convertModulesWithEnv
  , convertTaggedModulesInPackage
  , convertModulesInPackage
  , convertModulesInPackageWithEnv
  ) where

import Protolude hiding (check)

import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Control.Monad.Writer.Strict (runWriterT)
import qualified Data.Map as Map
import Data.String (String)

import Language.PureScript.Docs.Convert.ReExports (updateReExports)
import Language.PureScript.Docs.Convert.Single (convertSingleModule)
import Language.PureScript.Docs.Prim (primModules)
import Language.PureScript.Docs.Types
import qualified Language.PureScript as P

import Web.Bower.PackageMeta (PackageName)

import Text.Parsec (eof)

-- |
-- Like convertModuleInPackage, but with the modules tagged by their
-- file paths.
--
convertTaggedModulesInPackage ::
  (MonadError P.MultipleErrors m) =>
  [(FilePath, P.Module)] ->
  Map P.ModuleName PackageName ->
  m [(FilePath, Module)]
convertTaggedModulesInPackage taggedModules modulesDeps =
  traverse pairDocModule =<< convertModulesInPackage modules modulesDeps
  where
  modules = map snd taggedModules

  moduleNameToFileMap =
    Map.fromList $ swap . fmap P.getModuleName <$> taggedModules

  getModuleFile docModule =
    case Map.lookup (modName docModule) moduleNameToFileMap of
      Just filePath -> pure filePath
      Nothing -> throwError . P.errorMessage $
        P.ModuleNotFound $ modName docModule

  pairDocModule docModule = (, docModule) <$> getModuleFile docModule

-- |
-- Like convertModules, except that it takes a list of modules, together with
-- their dependency status, and discards dependency modules in the resulting
-- documentation.
--
convertModulesInPackage ::
  (MonadError P.MultipleErrors m) =>
  [P.Module] ->
  Map P.ModuleName PackageName ->
  m [Module]
convertModulesInPackage modules modulesDeps =
  fmap fst (convertModulesInPackageWithEnv modules modulesDeps)

convertModulesInPackageWithEnv ::
  (MonadError P.MultipleErrors m) =>
  [P.Module] ->
  Map P.ModuleName PackageName ->
  m ([Module], P.Env)
convertModulesInPackageWithEnv modules modulesDeps =
  go modules
  where
  go =
     convertModulesWithEnv withPackage
     >>> fmap (first (filter (shouldKeep . modName)))

  shouldKeep mn = isLocal mn && not (P.isBuiltinModuleName mn)

  withPackage :: P.ModuleName -> InPackage P.ModuleName
  withPackage mn =
    case Map.lookup mn modulesDeps of
      Just pkgName -> FromDep pkgName mn
      Nothing -> Local mn

  isLocal :: P.ModuleName -> Bool
  isLocal = not . flip Map.member modulesDeps

-- |
-- Convert a group of modules to the intermediate format, designed for
-- producing documentation from.
--
-- Note that the whole module dependency graph must be included in the list; if
-- some modules import things from other modules, then those modules must also
-- be included.
--
-- For value declarations, if explicit type signatures are omitted, or a
-- wildcard type is used, then we typecheck the modules and use the inferred
-- types.
--
convertModules ::
  (MonadError P.MultipleErrors m) =>
  (P.ModuleName -> InPackage P.ModuleName) ->
  [P.Module] ->
  m [Module]
convertModules withPackage =
  fmap fst . convertModulesWithEnv withPackage

convertModulesWithEnv ::
  (MonadError P.MultipleErrors m) =>
  (P.ModuleName -> InPackage P.ModuleName) ->
  [P.Module] ->
  m ([Module], P.Env)
convertModulesWithEnv withPackage =
  P.sortModules
    >>> fmap (fst >>> map P.importPrim)
    >=> convertSorted withPackage

-- |
-- Convert a sorted list of modules, returning both the list of converted
-- modules and the Env produced during desugaring.
--
convertSorted ::
  (MonadError P.MultipleErrors m) =>
  (P.ModuleName -> InPackage P.ModuleName) ->
  [P.Module] ->
  m ([Module], P.Env)
convertSorted withPackage modules = do
  (env, convertedModules) <- second (map convertSingleModule) <$> partiallyDesugar modules

  modulesWithTypes <- typeCheckIfNecessary modules convertedModules

  -- We add the Prim docs modules here, so that docs generation is still
  -- possible if the modules we are generating docs for re-export things from
  -- Prim submodules. Note that the Prim modules do not exist as
  -- @Language.PureScript.Module@ values because they do not contain anything
  -- that exists at runtime. However, we have pre-constructed
  -- @Language.PureScript.Docs.Types.Module@ values for them, which we use
  -- here.
  let moduleMap =
        Map.fromList
          (map (modName &&& identity)
               (modulesWithTypes ++ primModules))

  -- Set up the traversal order for re-export handling so that Prim modules
  -- come first.
  let primModuleNames = Map.keys P.primEnv
  let traversalOrder = primModuleNames ++ map P.getModuleName modules
  let withReExports = updateReExports env traversalOrder withPackage moduleMap
  pure (Map.elems withReExports, env)

-- |
-- If any exported value declarations have either wildcard type signatures, or
-- none at all, then typecheck in order to fill them in with the inferred
-- types.
--
typeCheckIfNecessary ::
  (MonadError P.MultipleErrors m) =>
  [P.Module] ->
  [Module] ->
  m [Module]
typeCheckIfNecessary modules convertedModules =
  if any hasWildcards convertedModules
    then go
    else pure convertedModules

  where
  hasWildcards = any (isWild . declInfo) . modDeclarations
  isWild (ValueDeclaration P.TypeWildcard{}) = True
  isWild _ = False

  go = do
    checkEnv <- snd <$> typeCheck modules
    pure (map (insertValueTypes checkEnv) convertedModules)

-- |
-- Typechecks all the modules together. Also returns the final 'P.Environment',
-- which is useful for adding in inferred types where explicit declarations
-- were not provided.
--
typeCheck ::
  (MonadError P.MultipleErrors m) =>
  [P.Module] ->
  m ([P.Module], P.Environment)
typeCheck =
  (P.desugar [] >=> check)
  >>> fmap (second P.checkEnv)
  >>> P.evalSupplyT 0
  >>> ignoreWarnings

  where
  check ms =
    runStateT
      (traverse P.typeCheckModule ms)
      (P.emptyCheckState P.initEnvironment)

  ignoreWarnings =
    fmap fst . runWriterT

-- |
-- Updates all the types of the ValueDeclarations inside the module based on
-- their types inside the given Environment.
--
insertValueTypes ::
  P.Environment -> Module -> Module
insertValueTypes env m =
  m { modDeclarations = map go (modDeclarations m) }
  where
  go (d@Declaration { declInfo = ValueDeclaration P.TypeWildcard{} }) =
    let
      ident = parseIdent (declTitle d)
      ty = lookupName ident
    in
      d { declInfo = ValueDeclaration ty }
  go other =
    other

  parseIdent =
    either (err . ("failed to parse Ident: " ++)) identity . runParser P.parseIdent

  lookupName name =
    let key = P.Qualified (Just (modName m)) name
    in case Map.lookup key (P.names env) of
      Just (ty, _, _) ->
        ty
      Nothing ->
        err ("name not found: " ++ show key)

  err msg =
    P.internalError ("Docs.Convert.insertValueTypes: " ++ msg)

runParser :: P.TokenParser a -> Text -> Either String a
runParser p s = either (Left . show) Right $ do
  ts <- P.lex "" s
  P.runTokenParser "" (p <* eof) ts

-- |
-- Partially desugar modules so that they are suitable for extracting
-- documentation information from.
--
partiallyDesugar ::
  (MonadError P.MultipleErrors m) =>
  [P.Module]
  -> m (P.Env, [P.Module])
partiallyDesugar = P.evalSupplyT 0 . desugar'
  where
  desugar' =
    traverse P.desugarDoModule
      >=> map P.desugarLetPatternModule
      >>> traverse P.desugarCasesModule
      >=> traverse P.desugarTypeDeclarationsModule
      >=> ignoreWarnings . P.desugarImportsWithEnv []
      >=> traverse (P.rebracketFiltered isInstanceDecl [])

  ignoreWarnings = fmap fst . runWriterT

  isInstanceDecl (P.TypeInstanceDeclaration {}) = True
  isInstanceDecl _ = False
