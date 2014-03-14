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

module Language.PureScript (module P, compile, MonadMake(..), make) where

import Language.PureScript.Values as P
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

import Data.List (intercalate)
import Data.Time.Clock
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad.State.Lazy
import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Map as M
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
compile opts ms = do
  sorted <- sortModules ms
  desugared <- desugar sorted
  (elaborated, env) <- runCheck $ forM desugared $ \(Module moduleName' decls exps) -> do
    modify (\s -> s { checkCurrentModule = Just moduleName' })
    Module moduleName' <$> typeCheckAll mainModuleIdent moduleName' decls <*> pure exps
  regrouped <- createBindingGroupsModule . collapseBindingGroupsModule $ elaborated
  let entryPoints = moduleNameFromString `map` optionsModules opts
  let elim = if null entryPoints then regrouped else eliminateDeadCode entryPoints regrouped
  let codeGenModules = moduleNameFromString `map` optionsCodeGenModules opts
  let modulesToCodeGen = if null codeGenModules then elim else filter (\(Module mn _ _) -> mn `elem` codeGenModules) elim
  let js = mapMaybe (flip (moduleToJs opts) env) modulesToCodeGen
  let exts = intercalate "\n" . map (`moduleToPs` env) $ modulesToCodeGen
  js' <- generateMain env opts js
  return (prettyPrintJS [wrapExportsContainer opts js'], exts, env)
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
make :: (Monad m, MonadMake m) => Options -> [(FilePath, Module)] -> m ()
make opts ms = do
  let filePathMap = M.fromList (map (\(fp, (Module mn _ _)) -> (mn, fp)) ms)

  desugared <- liftError $ do
    sorted <- sortModules (map snd ms)
    desugar sorted
  go initEnvironment filePathMap desugared

  where
  go :: (Monad m, MonadMake m) => Environment -> M.Map ModuleName FilePath -> [Module] -> m ()
  go _ _ [] = return ()
  go env filePathMap (Module moduleName' decls exps : ms') = do
    let filePath = toFileName moduleName'
    let jsFile = "js" ++ pathSeparator : filePath ++ ".js"
        externsFile = "externs" ++ pathSeparator : filePath ++ ".externs"

    jsTimestamp <- getTimestamp jsFile
    externsTimestamp <- getTimestamp externsFile

    let inputFile = fromMaybe (error "Input file is undefined in make") $ M.lookup moduleName' filePathMap
    inputTimestamp <- getTimestamp inputFile

    env' <- case () of
      _ | inputTimestamp < min jsTimestamp externsTimestamp -> do
            externs <- readTextFile externsFile
            externsModules <- liftError . either (Left . show) Right $ P.runIndentParser externsFile P.parseModules externs
            case externsModules of
              [m@(Module moduleName'' _ _)] | moduleName' == moduleName'' -> do
                [Module _ typings _] <- liftError $ desugar [m]
                (_, env') <- liftError . runCheck' env $ do
                  modify (\s -> s { checkCurrentModule = Just moduleName' })
                  typeCheckAll Nothing moduleName' typings
                return env'
              _ -> liftError . Left $ "Externs file " ++ externsFile ++ " was invalid"
        | otherwise -> do
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
            return env'
    go env' filePathMap ms'

  toFileName :: ModuleName -> FilePath
  toFileName (ModuleName ps) = intercalate [pathSeparator] . map runProperName $ ps
