-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- The main compiler module
--
-----------------------------------------------------------------------------

module Language.PureScript (module P, compile, compile', MonadMake(..), make, preludeFilename) where

import Language.PureScript.Types as P
import Language.PureScript.Kinds as P
import Language.PureScript.Declarations as P
import Language.PureScript.Names as P
import Language.PureScript.Parser as P
import Language.PureScript.CodeGen as P
import Language.PureScript.CodeGen.Common as P
import Language.PureScript.TypeChecker as P
import Language.PureScript.Pretty as P
import Language.PureScript.Sugar as P
import Language.PureScript.Options as P
import Language.PureScript.ModuleDependencies as P
import Language.PureScript.Environment as P
import Language.PureScript.Errors as P
import Language.PureScript.DeadCodeElimination as P
import Language.PureScript.Supply as P
import Language.PureScript.Renamer as P

import qualified Language.PureScript.Constants as C
import qualified Paths_purescript as Paths

import Data.List (find, sortBy, groupBy, intercalate)
import Data.Time.Clock
import Data.Function (on)
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad.Error
import Control.Monad.State.Lazy
import Control.Arrow ((&&&))
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import System.FilePath (pathSeparator)

-- |
-- Compile a collection of modules
--
-- The compilation pipeline proceeds as follows:
--
--  * Sort the modules based on module dependencies, checking for cyclic dependencies.
--
--  * Perform a set of desugaring passes.
--
--  * Type check, and elaborate values to include type annotations and type class dictionaries.
--
--  * Regroup values to take into account new value dependencies introduced by elaboration.
--
--  * Eliminate dead code.
--
--  * Generate Javascript, and perform optimization passes.
--
--  * Pretty-print the generated Javascript
--
compile :: Options -> [Module] -> Either String (String, String, Environment)
compile = compile' initEnvironment

compile' :: Environment -> Options -> [Module] -> Either String (String, String, Environment)
compile' env opts ms = do
  (sorted, _) <- sortModules $ map importPrim $ if optionsNoPrelude opts then ms else (map importPrelude ms)
  (desugared, nextVar) <- stringifyErrorStack True $ runSupplyT 0 $ desugar sorted
  (elaborated, env') <- runCheck' opts env $ forM desugared $ typeCheckModule mainModuleIdent
  regrouped <- stringifyErrorStack True $ createBindingGroupsModule . collapseBindingGroupsModule $ elaborated
  let entryPoints = moduleNameFromString `map` optionsModules opts
  let elim = if null entryPoints then regrouped else eliminateDeadCode entryPoints regrouped
  let renamed = renameInModules elim
  let codeGenModules = moduleNameFromString `map` optionsCodeGenModules opts
  let modulesToCodeGen = if null codeGenModules then renamed else filter (\(Module mn _ _) -> mn `elem` codeGenModules) renamed
  let js = evalSupply nextVar $ concat <$> mapM (\m -> moduleToJs Globals opts m env') modulesToCodeGen
  let exts = intercalate "\n" . map (`moduleToPs` env') $ modulesToCodeGen
  js' <- generateMain env' opts js
  return (prettyPrintJS js', exts, env')
  where
  mainModuleIdent = moduleNameFromString <$> optionsMain opts

typeCheckModule :: Maybe ModuleName -> Module -> Check Module
typeCheckModule mainModuleName (Module mn decls exps) = do
  modify (\s -> s { checkCurrentModule = Just mn })
  decls' <- typeCheckAll mainModuleName mn decls
  mapM_ checkTypesAreExported exps'
  return $ Module mn decls' exps
  where

  exps' = fromMaybe (error "exports should have been elaborated") exps

  -- Check that all the type constructors defined in the current module that appear in member types
  -- have also been exported from the module
  checkTypesAreExported :: DeclarationRef -> Check ()
  checkTypesAreExported (ValueRef name) = do
    ty <- lookupVariable mn (Qualified (Just mn) name)
    case find isTconHidden (findTcons ty) of
      Just hiddenType -> throwError . strMsg $
        "Error in module '" ++ show mn ++ "':\n\
        \Exporting declaration '" ++ show name ++ "' requires type '" ++ show hiddenType ++ "' to be exported as well"
      Nothing -> return ()
  checkTypesAreExported _ = return ()

  -- Find the type constructors exported from the current module used in a type
  findTcons :: Type -> [ProperName]
  findTcons = everythingOnTypes (++) go
    where
    go (TypeConstructor (Qualified (Just mn') name)) | mn' == mn = [name]
    go _ = []

  -- Checks whether a type constructor is not being exported from the current module
  isTconHidden :: ProperName -> Bool
  isTconHidden tyName = all go exps'
    where
    go (TypeRef tyName' _) = tyName' /= tyName
    go _ = True


generateMain :: Environment -> Options -> [JS] -> Either String [JS]
generateMain env opts js =
  case moduleNameFromString <$> optionsMain opts of
    Just mmi -> do
      when ((mmi, Ident C.main) `M.notMember` names env) $
        Left $ show mmi ++ "." ++ C.main ++ " is undefined"
      return $ js ++ [JSApp (JSAccessor C.main (JSAccessor (moduleNameToJs mmi) (JSVar (fromJust (optionsBrowserNamespace opts))))) []]
    _ -> return js

-- |
-- A type class which collects the IO actions we need to be able to run in "make" mode
--
class MonadMake m where
  -- |
  -- Get a file timestamp
  --
  getTimestamp :: FilePath -> m (Maybe UTCTime)

  -- |
  -- Read a file as a string
  --
  readTextFile :: FilePath -> m String

  -- |
  -- Write a text file
  --
  writeTextFile :: FilePath -> String -> m ()

  -- |
  -- Report an error
  --
  liftError :: Either String a -> m a

  -- |
  -- Respond to a progress update
  --
  progress :: String -> m ()

-- |
-- Compiles in "make" mode, compiling each module separately to a js files and an externs file
--
-- If timestamps have not changed, the externs file can be used to provide the module's types without
-- having to typecheck the module again.
--
make :: (Functor m, Applicative m, Monad m, MonadMake m) => FilePath -> Options -> [(FilePath, Module)] -> m Environment
make outputDir opts ms = do
  let filePathMap = M.fromList (map (\(fp, Module mn _ _) -> (mn, fp)) ms)

  (sorted, graph) <- liftError $ sortModules $ map importPrim $ if optionsNoPrelude opts then map snd ms else (map (importPrelude . snd) ms)

  toRebuild <- foldM (\s (Module moduleName' _ _) -> do
    let filePath = runModuleName moduleName'

        jsFile = outputDir ++ pathSeparator : filePath ++ pathSeparator : "index.js"
        externsFile = outputDir ++ pathSeparator : filePath ++ pathSeparator : "externs.purs"
        inputFile = fromMaybe (error "Input file is undefined in make") $ M.lookup moduleName' filePathMap

    jsTimestamp <- getTimestamp jsFile
    externsTimestamp <- getTimestamp externsFile
    inputTimestamp <- getTimestamp inputFile

    return $ case (inputTimestamp, jsTimestamp, externsTimestamp) of
      (Just t1, Just t2, Just t3) | t1 < min t2 t3 -> s
      _ -> S.insert moduleName' s) S.empty sorted

  marked <- rebuildIfNecessary (reverseDependencies graph) toRebuild sorted

  (desugared, nextVar) <- liftError $ stringifyErrorStack True $ runSupplyT 0 $ zip (map fst marked) <$> desugar (map snd marked)

  evalSupplyT nextVar (go initEnvironment desugared)

  where
  go :: (Functor m, Applicative m, Monad m, MonadMake m) => Environment -> [(Bool, Module)] -> SupplyT m Environment
  go env [] = return env
  go env ((False, m) : ms') = do
    (_, env') <- lift . liftError . runCheck' opts env $ typeCheckModule Nothing m

    go env' ms'
  go env ((True, m@(Module moduleName' _ exps)) : ms') = do
    let filePath = runModuleName moduleName'
        jsFile = outputDir ++ pathSeparator : filePath ++ pathSeparator : "index.js"
        externsFile = outputDir ++ pathSeparator : filePath ++ pathSeparator : "externs.purs"

    lift . progress $ "Compiling " ++ runModuleName moduleName'

    (Module _ elaborated _, env') <- lift . liftError . runCheck' opts env $ typeCheckModule Nothing m

    regrouped <- lift . liftError . stringifyErrorStack True . createBindingGroups moduleName' . collapseBindingGroups $ elaborated

    let mod' = Module moduleName' regrouped exps
    let [renamed] = renameInModules [mod'] 

    js <- prettyPrintJS <$> moduleToJs CommonJS opts renamed env'
    let exts = moduleToPs renamed env'

    lift $ writeTextFile jsFile js
    lift $ writeTextFile externsFile exts

    go env' ms'

  rebuildIfNecessary :: (Functor m, Monad m, MonadMake m) => M.Map ModuleName [ModuleName] -> S.Set ModuleName -> [Module] -> m [(Bool, Module)]
  rebuildIfNecessary _ _ [] = return []
  rebuildIfNecessary graph toRebuild (m@(Module moduleName' _ _) : ms') | moduleName' `S.member` toRebuild = do
    let deps = fromMaybe [] $ moduleName' `M.lookup` graph
        toRebuild' = toRebuild `S.union` S.fromList deps
    (:) (True, m) <$> rebuildIfNecessary graph toRebuild' ms'
  rebuildIfNecessary graph toRebuild (Module moduleName' _ _ : ms') = do
    let externsFile = outputDir ++ pathSeparator : runModuleName moduleName' ++ pathSeparator : "externs.purs"
    externs <- readTextFile externsFile
    externsModules <- liftError . either (Left . show) Right $ P.runIndentParser externsFile P.parseModules externs
    case externsModules of
      [m'@(Module moduleName'' _ _)] | moduleName'' == moduleName' -> (:) (False, m') <$> rebuildIfNecessary graph toRebuild ms'
      _ -> liftError . Left $ "Externs file " ++ externsFile ++ " was invalid"

reverseDependencies :: ModuleGraph -> M.Map ModuleName [ModuleName]
reverseDependencies g = combine [ (dep, mn) | (mn, deps) <- g, dep <- deps ]
  where
  combine :: (Ord a) => [(a, b)] -> M.Map a [b]
  combine = M.fromList . map ((fst . head) &&& map snd) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

-- |
-- Add an import declaration for a module if it does not already explicitly import it.
--
addDefaultImport :: ModuleName -> Module -> Module
addDefaultImport toImport m@(Module mn decls exps)  =
  if isExistingImport `any` decls || mn == toImport then m
  else Module mn (ImportDeclaration toImport Unqualified Nothing : decls) exps
  where
  isExistingImport (ImportDeclaration mn' _ _) | mn' == toImport = True
  isExistingImport (PositionedDeclaration _ d) = isExistingImport d
  isExistingImport _ = False

importPrim :: Module -> Module
importPrim = addDefaultImport (ModuleName [ProperName C.prim])

importPrelude :: Module -> Module
importPrelude = addDefaultImport (ModuleName [ProperName C.prelude])

preludeFilename :: IO FilePath
preludeFilename = Paths.getDataFileName "prelude/prelude.purs"
