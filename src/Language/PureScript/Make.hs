-----------------------------------------------------------------------------
--
-- Module      :  Make
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Make
  (
  -- * Make API
    RebuildPolicy(..)
  , ProgressMessage(..), renderProgressMessage
  , MakeActions(..)
  , Externs()
  , make

  -- * Implementation of Make API using files on disk
  , Make(..)
  , runMake
  , buildMakeActions
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Control.Monad.Supply

import Data.Function (on)
import Data.List (sortBy, groupBy)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Foldable (for_)
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (traverse)
#endif
import Data.Version (showVersion)
import qualified Data.Map as M
import qualified Data.Set as S

import System.Directory
       (doesFileExist, getModificationTime, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (tryIOError)

import Language.PureScript.AST
import Language.PureScript.CodeGen.Externs (moduleToPs)
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Linter
import Language.PureScript.ModuleDependencies
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.Parser
import Language.PureScript.Pretty
import Language.PureScript.Renamer
import Language.PureScript.Sugar
import Language.PureScript.TypeChecker
import qualified Language.PureScript.Constants as C

import qualified Language.PureScript.CodeGen.JS as J
import qualified Language.PureScript.CoreFn as CF
import qualified Paths_purescript as Paths

-- | Progress messages from the make process
data ProgressMessage
  = CompilingModule ModuleName
  deriving (Show, Read, Eq, Ord)

-- | Render a progress message
renderProgressMessage :: ProgressMessage -> String
renderProgressMessage (CompilingModule mn) = "Compiling " ++ runModuleName mn

-- | Actions that require implementations when running in "make" mode.
--
-- This type exists to make two things abstract:
--
-- * The particular backend being used (Javascript, C++11, etc.)
--
-- * The details of how files are read/written etc.
--
data MakeActions m = MakeActions {
  -- |
  -- Get the timestamp for the input file(s) for a module. If there are multiple
  -- files (.purs and foreign files, for example) the timestamp should be for
  -- the most recently modified file.
  --
    getInputTimestamp :: ModuleName -> m (Either RebuildPolicy (Maybe UTCTime))
  -- |
  -- Get the timestamp for the output files for a module. This should be the
  -- timestamp for the oldest modified file, or Nothing if any of the required
  -- output files are missing.
  --
  , getOutputTimestamp :: ModuleName -> m (Maybe UTCTime)
  -- |
  -- Read the externs file for a module as a string and also return the actual
  -- path for the file.
  , readExterns :: ModuleName -> m (FilePath, String)
  -- |
  -- Run the code generator for the module and write any required output files.
  --
  , codegen :: CF.Module CF.Ann -> Environment -> Externs -> SupplyT m ()
  -- |
  -- Respond to a progress update.
  --
  , progress :: ProgressMessage -> m ()
  }

-- |
-- Generated code for an externs file.
--
type Externs = String

-- |
-- Determines when to rebuild a module
--
data RebuildPolicy
  -- | Never rebuild this module
  = RebuildNever
  -- | Always rebuild this module
  | RebuildAlways deriving (Show, Read, Eq, Ord)

-- |
-- Compiles in "make" mode, compiling each module separately to a js files and an externs file
--
-- If timestamps have not changed, the externs file can be used to provide the module's types without
-- having to typecheck the module again.
--
make :: forall m. (Functor m, Applicative m, Monad m, MonadReader Options m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [Module]
     -> m Environment
make MakeActions{..} ms = do
  (sorted, graph) <- sortModules $ map importPrim ms
  toRebuild <- foldM (\s (Module _ _ moduleName' _ _) -> do
    inputTimestamp <- getInputTimestamp moduleName'
    outputTimestamp <- getOutputTimestamp moduleName'
    return $ case (inputTimestamp, outputTimestamp) of
      (Right (Just t1), Just t2) | t1 < t2 -> s
      (Left RebuildNever, Just _) -> s
      _ -> S.insert moduleName' s) S.empty sorted

  marked <- rebuildIfNecessary (reverseDependencies graph) toRebuild sorted
  for_ marked $ \(willRebuild, m) -> when willRebuild (lint m)
  (desugared, nextVar) <- runSupplyT 0 $ zip (map fst marked) <$> desugar (map snd marked)
  evalSupplyT nextVar $ go initEnvironment desugared
  where

  go :: Environment -> [(Bool, Module)] -> SupplyT m Environment
  go env [] = return env
  go env ((False, m) : ms') = do
    (_, env') <- lift . runCheck' env $ typeCheckModule m
    go env' ms'
  go env ((True, m@(Module ss coms moduleName' _ exps)) : ms') = do
    lift . progress $ CompilingModule moduleName'
    (checked@(Module _ _ _ elaborated _), env') <- lift . runCheck' env $ typeCheckModule m
    checkExhaustiveModule env' checked
    regrouped <- createBindingGroups moduleName' . collapseBindingGroups $ elaborated
    let mod' = Module ss coms moduleName' regrouped exps
        corefn = CF.moduleToCoreFn env' mod'
        [renamed] = renameInModules [corefn]
        exts = moduleToPs mod' env'
    codegen renamed env' exts
    go env' ms'

  rebuildIfNecessary :: M.Map ModuleName [ModuleName] -> S.Set ModuleName -> [Module] -> m [(Bool, Module)]
  rebuildIfNecessary _ _ [] = return []
  rebuildIfNecessary graph toRebuild (m@(Module _ _ moduleName' _ _) : ms') | moduleName' `S.member` toRebuild = do
    let deps = fromMaybe [] $ moduleName' `M.lookup` graph
        toRebuild' = toRebuild `S.union` S.fromList deps
    (:) (True, m) <$> rebuildIfNecessary graph toRebuild' ms'
  rebuildIfNecessary graph toRebuild (Module _ _ moduleName' _ _ : ms') = do
    (path, externs) <- readExterns moduleName'
    externsModules <- fmap (map snd) . alterErrors $ parseModulesFromFiles id [(path, externs)]
    case externsModules of
      [m'@(Module _ _ moduleName'' _ _)] | moduleName'' == moduleName' -> (:) (False, m') <$> rebuildIfNecessary graph toRebuild ms'
      _ -> throwError . errorMessage . InvalidExternsFile $ path
    where
    alterErrors = flip catchError $ \(MultipleErrors errs) ->
      throwError . MultipleErrors $ flip map errs $ \e -> case e of
        SimpleErrorWrapper (ErrorParsingModule err) -> SimpleErrorWrapper (ErrorParsingExterns err)
        _ -> e

reverseDependencies :: ModuleGraph -> M.Map ModuleName [ModuleName]
reverseDependencies g = combine [ (dep, mn) | (mn, deps) <- g, dep <- deps ]
  where
  combine :: (Ord a) => [(a, b)] -> M.Map a [b]
  combine = M.fromList . map ((fst . head) &&& map snd) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

-- |
-- Add an import declaration for a module if it does not already explicitly import it.
--
addDefaultImport :: ModuleName -> Module -> Module
addDefaultImport toImport m@(Module ss coms mn decls exps)  =
  if isExistingImport `any` decls || mn == toImport then m
  else Module ss coms mn (ImportDeclaration toImport Implicit Nothing : decls) exps
  where
  isExistingImport (ImportDeclaration mn' _ _) | mn' == toImport = True
  isExistingImport (PositionedDeclaration _ _ d) = isExistingImport d
  isExistingImport _ = False

importPrim :: Module -> Module
importPrim = addDefaultImport (ModuleName [ProperName C.prim])

-- |
-- A monad for running make actions
--
newtype Make a = Make { unMake :: ReaderT Options (WriterT MultipleErrors (ExceptT MultipleErrors IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError MultipleErrors, MonadWriter MultipleErrors, MonadReader Options)

-- |
-- Execute a 'Make' monad, returning either errors, or the result of the compile plus any warnings.
--
runMake :: Options -> Make a -> IO (Either MultipleErrors (a, MultipleErrors))
runMake opts = runExceptT . runWriterT . flip runReaderT opts . unMake

makeIO :: (IOError -> ErrorMessage) -> IO a -> Make a
makeIO f io = do
  e <- liftIO $ tryIOError io
  either (throwError . singleError . f) return e

-- Traverse (Either e) instance (base 4.7)
traverseEither :: Applicative f => (a -> f b) -> Either e a -> f (Either e b)
traverseEither _ (Left x) = pure (Left x)
traverseEither f (Right y) = Right <$> f y

-- |
-- A set of make actions that read and write modules from the given directory.
--
buildMakeActions :: FilePath -- ^ the output directory
                 -> M.Map ModuleName (Either RebuildPolicy FilePath) -- ^ a map between module names and paths to the file containing the PureScript module
                 -> M.Map ModuleName FilePath -- ^ a map between module name and the file containing the foreign javascript for the module
                 -> Bool -- ^ Generate a prefix comment?
                 -> MakeActions Make
buildMakeActions outputDir filePathMap foreigns usePrefix =
  MakeActions getInputTimestamp getOutputTimestamp readExterns codegen progress
  where

  getInputTimestamp :: ModuleName -> Make (Either RebuildPolicy (Maybe UTCTime))
  getInputTimestamp mn = do
    let path = fromMaybe (error "Module has no filename in 'make'") $ M.lookup mn filePathMap
    e1 <- traverseEither getTimestamp path
    fPath <- maybe (return Nothing) getTimestamp $ M.lookup mn foreigns
    return $ fmap (max fPath) e1

  getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    let filePath = runModuleName mn
        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.purs"
    min <$> getTimestamp jsFile <*> getTimestamp externsFile

  readExterns :: ModuleName -> Make (FilePath, String)
  readExterns mn = do
    let path = outputDir </> runModuleName mn </> "externs.purs"
    (path, ) <$> readTextFile path

  codegen :: CF.Module CF.Ann -> Environment -> Externs -> SupplyT Make ()
  codegen m _ exts = do
    let mn = CF.moduleName m
    foreignInclude <- case mn `M.lookup` foreigns of
      Just path
        | not $ requiresForeign m -> do
            tell $ errorMessage $ UnnecessaryFFIModule mn path
            return Nothing
        | otherwise -> return $ Just $ J.JSApp (J.JSVar "require") [J.JSStringLiteral "./foreign"]
      Nothing | requiresForeign m -> throwError . errorMessage $ MissingFFIModule mn
              | otherwise -> return Nothing
    pjs <- prettyPrintJS <$> J.moduleToJs m foreignInclude
    let filePath = runModuleName mn
        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.purs"
        foreignFile = outputDir </> filePath </> "foreign.js"
        prefix = ["Generated by psc version " ++ showVersion Paths.version | usePrefix]
        js = unlines $ map ("// " ++) prefix ++ [pjs]
    lift $ do
      writeTextFile jsFile js
      for_ (mn `M.lookup` foreigns) (readTextFile >=> writeTextFile foreignFile)
      writeTextFile externsFile exts

  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign

  getTimestamp :: FilePath -> Make (Maybe UTCTime)
  getTimestamp path = makeIO (const (SimpleErrorWrapper $ CannotGetFileInfo path)) $ do
    exists <- doesFileExist path
    traverse (const $ getModificationTime path) $ guard exists

  readTextFile :: FilePath -> Make String
  readTextFile path = makeIO (const (SimpleErrorWrapper $ CannotReadFile path)) $ readFile path

  writeTextFile :: FilePath -> String -> Make ()
  writeTextFile path text = makeIO (const (SimpleErrorWrapper $ CannotWriteFile path)) $ do
    mkdirp path
    writeFile path text
    where
    mkdirp :: FilePath -> IO ()
    mkdirp = createDirectoryIfMissing True . takeDirectory

  progress :: ProgressMessage -> Make ()
  progress = liftIO . putStrLn . renderProgressMessage
