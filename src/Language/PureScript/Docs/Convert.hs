-- | Functions for converting PureScript ASTs into values of the data types
-- from Language.PureScript.Docs.

module Language.PureScript.Docs.Convert
  ( convertModules
  , convertModulesWithEnv
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
import Language.PureScript.Docs.Types
import qualified Language.PureScript as P

import Web.Bower.PackageMeta (PackageName)

import Text.Parsec (eof)

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
     >>> fmap (first (filter (isLocal . modName)))

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
  let moduleMap = Map.fromList (map (modName &&& identity) modulesWithTypes)

  let traversalOrder = map P.getModuleName modules
  pure (Map.elems (updateReExports env traversalOrder withPackage moduleMap), env)

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
