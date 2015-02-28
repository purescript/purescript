-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript
  ( module P
  , compile
  , compile'
  , RebuildPolicy(..)
  , MonadMake(..)
  , make
  , prelude
  , version
  ) where

import Data.FileEmbed (embedFile)
import Data.Function (on)
import Data.List (sortBy, groupBy, intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Version (Version)
import qualified Data.Traversable as T (traverse)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad.Except
import Control.Monad.Reader

import System.FilePath ((</>))

import Language.PureScript.AST as P
import Language.PureScript.Comments as P
import Language.PureScript.CodeGen as P
import Language.PureScript.DeadCodeElimination as P
import Language.PureScript.Environment as P
import Language.PureScript.Errors as P
import Language.PureScript.Kinds as P
import Language.PureScript.ModuleDependencies as P
import Language.PureScript.Names as P
import Language.PureScript.Options as P
import Language.PureScript.Parser as P
import Language.PureScript.Pretty as P
import Language.PureScript.Renamer as P
import Language.PureScript.Sugar as P
import Control.Monad.Supply as P
import Language.PureScript.TypeChecker as P
import Language.PureScript.Types as P
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.CoreImp as CI
import qualified Language.PureScript.Constants as C

import qualified Paths_purescript as Paths

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
compile :: (Functor m, Applicative m, MonadError MultipleErrors m, MonadReader (Options Compile) m)
        => [Module] -> [String] -> m (String, String, Environment)
compile = compile' initEnvironment

compile' :: (Functor m, Applicative m, MonadError MultipleErrors m, MonadReader (Options Compile) m)
         => Environment -> [Module] -> [String] -> m (String, String, Environment)
compile' env ms prefix = do
  noPrelude <- asks optionsNoPrelude
  additional <- asks optionsAdditional
  mainModuleIdent <- asks (fmap moduleNameFromString . optionsMain)
  (sorted, _) <- sortModules $ map importPrim $ if noPrelude then ms else map importPrelude ms
  (desugared, nextVar) <- runSupplyT 0 $ desugar sorted
  (elaborated, env') <- runCheck' env $ forM desugared $ typeCheckModule mainModuleIdent
  regrouped <- createBindingGroupsModule . collapseBindingGroupsModule $ elaborated
  let corefn = map (CF.moduleToCoreFn env') regrouped
  let entryPoints = moduleNameFromString `map` entryPointModules additional
  let elim = if null entryPoints then corefn else eliminateDeadCode entryPoints corefn
  let renamed = renameInModules elim
  let codeGenModuleNames = moduleNameFromString `map` codeGenModules additional
  let modulesToCodeGen = if null codeGenModuleNames then renamed else filter (\(CF.Module _ mn _ _ _ _) -> mn `elem` codeGenModuleNames) renamed
  js <- concat <$> evalSupplyT nextVar (T.traverse (CI.moduleToCoreImp >=> moduleToJs) modulesToCodeGen)
  let exts = intercalate "\n" . map (`moduleToPs` env') $ regrouped
  js' <- generateMain env' js
  let pjs = unlines $ map ("// " ++) prefix ++ [prettyPrintJS js']
  return (pjs, exts, env')

generateMain :: (MonadError MultipleErrors m, MonadReader (Options Compile) m) => Environment -> [JS] -> m [JS]
generateMain env js = do
  main <- asks optionsMain
  additional <- asks optionsAdditional
  case moduleNameFromString <$> main of
    Just mmi -> do
      when ((mmi, Ident C.main) `M.notMember` names env) $
        throwError . errorMessage $ NameIsUndefined (Ident C.main)
      return $ js ++ [JSApp (JSAccessor C.main (JSAccessor (moduleNameToJs mmi) (JSVar (browserNamespace additional)))) []]
    _ -> return js

-- |
-- A type class which collects the IO actions we need to be able to run in "make" mode
--
class (MonadReader (P.Options P.Make) m, MonadError MultipleErrors m) => MonadMake m where
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
  -- Respond to a progress update
  --
  progress :: String -> m ()

-- |
-- Determines when to rebuild a module
--
data RebuildPolicy
  -- | Never rebuild this module
  = RebuildNever
  -- | Always rebuild this module
  | RebuildAlways deriving (Show, Eq, Ord)

-- Traverse (Either e) instance (base 4.7)
traverseEither :: Applicative f => (a -> f b) -> Either e a -> f (Either e b)
traverseEither _ (Left x) = pure (Left x)
traverseEither f (Right y) = Right <$> f y

-- |
-- Compiles in "make" mode, compiling each module separately to a js files and an externs file
--
-- If timestamps have not changed, the externs file can be used to provide the module's types without
-- having to typecheck the module again.
--
make :: forall m. (Functor m, Applicative m, Monad m, MonadMake m)
     => FilePath -> [(Either RebuildPolicy FilePath, Module)] -> [String] -> m Environment
make outputDir ms prefix = do
  noPrelude <- asks optionsNoPrelude
  let filePathMap = M.fromList (map (\(fp, Module _ mn _ _) -> (mn, fp)) ms)

  (sorted, graph) <- sortModules $ map importPrim $ if noPrelude then map snd ms else map (importPrelude . snd) ms

  toRebuild <- foldM (\s (Module _ moduleName' _ _) -> do
    let filePath = runModuleName moduleName'

        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.purs"
        inputFile = fromMaybe (error "Module has no filename in 'make'") $ M.lookup moduleName' filePathMap

    jsTimestamp <- getTimestamp jsFile
    externsTimestamp <- getTimestamp externsFile
    inputTimestamp <- traverseEither getTimestamp inputFile

    return $ case (inputTimestamp, jsTimestamp, externsTimestamp) of
      (Right (Just t1), Just t2, Just t3) | t1 < min t2 t3 -> s
      (Left RebuildNever, Just _, Just _) -> s
      _ -> S.insert moduleName' s) S.empty sorted

  marked <- rebuildIfNecessary (reverseDependencies graph) toRebuild sorted

  (desugared, nextVar) <- runSupplyT 0 $ zip (map fst marked) <$> desugar (map snd marked)

  evalSupplyT nextVar $ go initEnvironment desugared

  where
  go :: Environment -> [(Bool, Module)] -> SupplyT m Environment
  go env [] = return env
  go env ((False, m) : ms') = do
    (_, env') <- lift . runCheck' env $ typeCheckModule Nothing m

    go env' ms'
  go env ((True, m@(Module coms moduleName' _ exps)) : ms') = do
    let filePath = runModuleName moduleName'
        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.purs"

    lift . progress $ "Compiling " ++ runModuleName moduleName'

    (Module _ _ elaborated _, env') <- lift . runCheck' env $ typeCheckModule Nothing m

    regrouped <- createBindingGroups moduleName' . collapseBindingGroups $ elaborated

    let mod' = Module coms moduleName' regrouped exps
    let corefn = CF.moduleToCoreFn env' mod'
    let [renamed] = renameInModules [corefn]

    pjs <- prettyPrintJS <$> (CI.moduleToCoreImp >=> moduleToJs) renamed
    let js = unlines $ map ("// " ++) prefix ++ [pjs]
    let exts = unlines $ map ("-- " ++) prefix ++ [moduleToPs mod' env']

    lift $ writeTextFile jsFile js
    lift $ writeTextFile externsFile exts

    go env' ms'

  rebuildIfNecessary :: M.Map ModuleName [ModuleName] -> S.Set ModuleName -> [Module] -> m [(Bool, Module)]
  rebuildIfNecessary _ _ [] = return []
  rebuildIfNecessary graph toRebuild (m@(Module _ moduleName' _ _) : ms') | moduleName' `S.member` toRebuild = do
    let deps = fromMaybe [] $ moduleName' `M.lookup` graph
        toRebuild' = toRebuild `S.union` S.fromList deps
    (:) (True, m) <$> rebuildIfNecessary graph toRebuild' ms'
  rebuildIfNecessary graph toRebuild (Module _ moduleName' _ _ : ms') = do
    let externsFile = outputDir </> runModuleName moduleName' </> "externs.purs"
    externs <- readTextFile externsFile
    externsModules <- fmap (map snd) . either (throwError . errorMessage . ErrorParsingExterns) return $ P.parseModulesFromFiles id [(externsFile, externs)]
    case externsModules of
      [m'@(Module _ moduleName'' _ _)] | moduleName'' == moduleName' -> (:) (False, m') <$> rebuildIfNecessary graph toRebuild ms'
      _ -> throwError . errorMessage . InvalidExternsFile $ externsFile

reverseDependencies :: ModuleGraph -> M.Map ModuleName [ModuleName]
reverseDependencies g = combine [ (dep, mn) | (mn, deps) <- g, dep <- deps ]
  where
  combine :: (Ord a) => [(a, b)] -> M.Map a [b]
  combine = M.fromList . map ((fst . head) &&& map snd) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

-- |
-- Add an import declaration for a module if it does not already explicitly import it.
--
addDefaultImport :: ModuleName -> Module -> Module
addDefaultImport toImport m@(Module coms mn decls exps)  =
  if isExistingImport `any` decls || mn == toImport then m
  else Module coms mn (ImportDeclaration toImport Implicit Nothing : decls) exps
  where
  isExistingImport (ImportDeclaration mn' _ _) | mn' == toImport = True
  isExistingImport (PositionedDeclaration _ _ d) = isExistingImport d
  isExistingImport _ = False

importPrim :: Module -> Module
importPrim = addDefaultImport (ModuleName [ProperName C.prim])

importPrelude :: Module -> Module
importPrelude = addDefaultImport (ModuleName [ProperName C.prelude])

prelude :: String
prelude = BU.toString $(embedFile "prelude/prelude.purs")

version :: Version
version = Paths.version
