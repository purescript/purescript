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

module Language.PureScript (module P, compile, compile', MonadMake(..), make) where

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
import Language.PureScript.DeadCodeElimination as P

import qualified Language.PureScript.Constants as C

import Data.List (sortBy, groupBy, intercalate)
import Data.Time.Clock
import Data.Function (on)
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad.State.Lazy
import Control.Arrow ((&&&))
import Control.Applicative ((<$>), (<*>), pure)
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
  (sorted, _) <- sortModules (map importPrelude ms)
  desugared <- desugar sorted
  (elaborated, env') <- runCheck' env $ forM desugared $ \(Module moduleName' decls exps) -> do
    modify (\s -> s { checkCurrentModule = Just moduleName' })
    Module moduleName' <$> typeCheckAll mainModuleIdent moduleName' decls <*> pure exps
  regrouped <- createBindingGroupsModule . collapseBindingGroupsModule $ elaborated
  let entryPoints = moduleNameFromString `map` optionsModules opts
  let elim = if null entryPoints then regrouped else eliminateDeadCode entryPoints regrouped
  let codeGenModules = moduleNameFromString `map` optionsCodeGenModules opts
  let modulesToCodeGen = if null codeGenModules then elim else filter (\(Module mn _ _) -> mn `elem` codeGenModules) elim
  let js = mapMaybe (flip (moduleToJs opts) env') modulesToCodeGen
  let exts = intercalate "\n" . map (`moduleToPs` env') $ modulesToCodeGen
  js' <- generateMain env' opts js
  return (prettyPrintJS [wrapExportsContainer opts js'], exts, env')
  where
  mainModuleIdent = moduleNameFromString <$> optionsMain opts

generateMain :: Environment -> Options -> [JS] -> Either String [JS]
generateMain env opts js =
  case moduleNameFromString <$> optionsMain opts of
    Just mmi -> do
      when ((mmi, Ident C.main) `M.notMember` names env) $
        Left $ show mmi ++ "." ++ C.main ++ " is undefined"
      return $ js ++ [JSApp (JSAccessor C.main (JSAccessor (moduleNameToJs mmi) (JSVar C._ps))) []]
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
-- Compiles in "make" mode, compiling each module separately to a js files and an externs file
--
-- If timestamps have not changed, the externs file can be used to provide the module's types without
-- having to typecheck the module again.
--
make :: (Functor m, Monad m, MonadMake m) => Options -> [(FilePath, Module)] -> m ()
make opts ms = do
  let filePathMap = M.fromList (map (\(fp, (Module mn _ _)) -> (mn, fp)) ms)

  (sorted, graph) <- liftError $ sortModules (map (importPrelude . snd) ms)

  toRebuild <- foldM (\s (Module moduleName' _ _) -> do
    let filePath = toFileName moduleName'

        jsFile = "js" ++ pathSeparator : filePath ++ ".js"
        externsFile = "externs" ++ pathSeparator : filePath ++ ".externs"
        inputFile = fromMaybe (error "Input file is undefined in make") $ M.lookup moduleName' filePathMap

    jsTimestamp <- getTimestamp jsFile
    externsTimestamp <- getTimestamp externsFile
    inputTimestamp <- getTimestamp inputFile

    return $ case (inputTimestamp, jsTimestamp, externsTimestamp) of
      (Just t1, Just t2, Just t3) | t1 < min t2 t3 -> s
      _ -> S.insert moduleName' s) S.empty sorted

  marked <- rebuildIfNecessary (reverseDependencies graph) toRebuild sorted

  desugared <- liftError $ zip (map fst marked) <$> desugar (map snd marked)

  go initEnvironment desugared

  where
  go :: (Functor m, Monad m, MonadMake m) => Environment -> [(Bool, Module)] -> m ()
  go _ [] = return ()
  go env ((False, Module moduleName' typings _) : ms') = do
    (_, env') <- liftError . runCheck' env $ do
      modify (\s -> s { checkCurrentModule = Just moduleName' })
      typeCheckAll Nothing moduleName' typings

    go env' ms'
  go env ((True, Module moduleName' decls exps) : ms') = do
    let filePath = toFileName moduleName'
        jsFile = "js" ++ pathSeparator : filePath ++ ".js"
        externsFile = "externs" ++ pathSeparator : filePath ++ ".externs"

    (elaborated, env') <- liftError . runCheck' env $ do
      modify (\s -> s { checkCurrentModule = Just moduleName' })
      typeCheckAll Nothing moduleName' decls

    regrouped <- liftError . createBindingGroups moduleName' . collapseBindingGroups $ elaborated

    let mod' = Module moduleName' regrouped exps
        js = moduleToJs opts mod' env'
        exts = moduleToPs mod' env'
        js' = maybe "" (prettyPrintJS . return . wrapExportsContainer opts . return) js

    writeTextFile jsFile js'
    writeTextFile externsFile exts

    go env' ms'

toFileName :: ModuleName -> FilePath
toFileName (ModuleName ps) = intercalate [pathSeparator] . map runProperName $ ps

rebuildIfNecessary :: (Functor m, Monad m, MonadMake m) => M.Map ModuleName [ModuleName] -> S.Set ModuleName -> [Module] -> m [(Bool, Module)]
rebuildIfNecessary _ _ [] = return []
rebuildIfNecessary graph toRebuild (m@(Module moduleName' _ _) : ms) | moduleName' `S.member` toRebuild = do
  let deps = fromMaybe [] $ moduleName' `M.lookup` graph
      toRebuild' = toRebuild `S.union` S.fromList deps
  (:) (True, m) <$> rebuildIfNecessary graph toRebuild' ms
rebuildIfNecessary graph toRebuild (Module moduleName' _ _ : ms) = do
  let externsFile = "externs" ++ pathSeparator : toFileName moduleName' ++ ".externs"
  externs <- readTextFile externsFile
  externsModules <- liftError . either (Left . show) Right $ P.runIndentParser externsFile P.parseModules externs
  case externsModules of
    [m'@(Module moduleName'' _ _)] | moduleName'' == moduleName' -> (:) (False, m') <$> rebuildIfNecessary graph toRebuild ms
    _ -> liftError . Left $ "Externs file " ++ externsFile ++ " was invalid"

reverseDependencies :: ModuleGraph -> M.Map ModuleName [ModuleName]
reverseDependencies g = combine [ (dep, mn) | (mn, deps) <- g, dep <- deps ]
  where
  combine :: (Ord a) => [(a, b)] -> M.Map a [b]
  combine = M.fromList . map ((fst . head) &&& map snd) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

-- |
-- Add an import declaration for the Prelude to a module if it does not already explicitly import
-- it.
--
importPrelude :: Module -> Module
importPrelude m@(Module mn decls exps)  =
  if isPreludeImport `any` decls || mn == prelude then m
  else Module mn (preludeImport : decls) exps
  where
  prelude = ModuleName [ProperName C.prelude]
  isPreludeImport (ImportDeclaration (ModuleName [ProperName mn']) _ _) | mn' == C.prelude = True
  isPreludeImport _ = False
  preludeImport = ImportDeclaration prelude Nothing Nothing
