-----------------------------------------------------------------------------
--
-- Module      :  Make
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
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

module Language.PureScript.Make
  (
  -- * Make API
    RebuildPolicy(..)
  , MakeActions(..)
  , Externs()
  , make
  
  -- * Implementation of Make API using files on disk
  , Make(..)
  , runMake
  , buildMakeActions
  ) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Supply

import Data.Function (on)
import Data.Functor (($>))
import Data.List (sortBy, groupBy)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
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
  | RebuildAlways deriving (Show, Eq, Ord)

-- |
-- Actions that require implementations when running in "make" mode.
--
data MakeActions m = MakeActions {
  -- | Get the timestamp for the input file(s) for a module. If there are multiple
  -- files (.purs and foreign files, for example) the timestamp should be for
  -- the most recently modified file.
    getInputTimestamp :: ModuleName -> m (Either RebuildPolicy (Maybe UTCTime))
  -- | Get the timestamp for the output files for a module. This should be the
  -- timestamp for the oldest modified file, or Nothing if any of the required
  -- output files are missing.
  , getOutputTimestamp :: ModuleName -> m (Maybe UTCTime)
  -- | Read the externs file for a module as a string and also return the actual
  -- path for the file.
  , readExterns :: ModuleName -> m (FilePath, String)
  -- | Get the foreign function definitions for a module.
  , readForeignModule :: ModuleName -> m (Maybe ForeignJS)
  -- | Write output for a module to the correct files.
  , writeModule :: ModuleName -> String -> Externs -> Maybe ForeignJS -> m ()
  -- | Respond to a progress update.
  , progress :: String -> m ()
  }

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

-- |
-- Compiles in "make" mode, compiling each module separately to a js files and an externs file
--
-- If timestamps have not changed, the externs file can be used to provide the module's types without
-- having to typecheck the module again.
--
make :: forall m. (Functor m, Applicative m, Monad m, MonadReader Options m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [Module]
     -> Bool -- ^ Include a prefix in the generated JS
     -> m Environment
make MakeActions{..} ms usePrefix = do
  (sorted, graph) <- sortModules $ map importPrim ms
  mapM_ lint sorted
  toRebuild <- foldM (\s (Module _ moduleName' _ _) -> do
    inputTimestamp <- getInputTimestamp moduleName'
    outputTimestamp <- getOutputTimestamp moduleName'
    return $ case (inputTimestamp, outputTimestamp) of
      (Right (Just t1), Just t2) | t1 < t2 -> s
      (Left RebuildNever, Just _) -> s
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
    lift $ progress $ "Compiling " ++ runModuleName moduleName'
    (checked@(Module _ _ elaborated _), env') <- lift . runCheck' env $ typeCheckModule Nothing m
    checkExhaustiveModule env' checked
    regrouped <- createBindingGroups moduleName' . collapseBindingGroups $ elaborated
    let mod' = Module coms moduleName' regrouped exps
        corefn = CF.moduleToCoreFn env' mod'
        [renamed] = renameInModules [corefn]
        exts = moduleToPs mod' env'
    
    foreignModule <- lift $ readForeignModule moduleName'
    foreignInclude <- case (foreignModule, requiresForeign renamed) of
      (Just _, True) -> return $ Just $ J.JSApp (J.JSVar "require") [J.JSStringLiteral "./foreign"]
      (Nothing, False) -> return Nothing
      (Just _, False) -> tell (errorMessage $ UnnecessaryFFIModule moduleName') $> Nothing
      (Nothing, True) -> throwError . errorMessage $ MissingFFIModule moduleName'
    
    pjs <- prettyPrintJS <$> J.moduleToJs renamed foreignInclude
    let prefix = ["Generated by psc version " ++ showVersion Paths.version | usePrefix]
        js = unlines $ map ("// " ++) prefix ++ [pjs]
    
    lift $ writeModule moduleName' js exts foreignModule
    
    go env' ms'

  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign

  rebuildIfNecessary :: M.Map ModuleName [ModuleName] -> S.Set ModuleName -> [Module] -> m [(Bool, Module)]
  rebuildIfNecessary _ _ [] = return []
  rebuildIfNecessary graph toRebuild (m@(Module _ moduleName' _ _) : ms') | moduleName' `S.member` toRebuild = do
    let deps = fromMaybe [] $ moduleName' `M.lookup` graph
        toRebuild' = toRebuild `S.union` S.fromList deps
    (:) (True, m) <$> rebuildIfNecessary graph toRebuild' ms'
  rebuildIfNecessary graph toRebuild (Module _ moduleName' _ _ : ms') = do
    (path, externs) <- readExterns moduleName'
    externsModules <- fmap (map snd) . alterErrors $ parseModulesFromFiles id [(path, externs)]
    case externsModules of
      [m'@(Module _ moduleName'' _ _)] | moduleName'' == moduleName' -> (:) (False, m') <$> rebuildIfNecessary graph toRebuild ms'
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
  addDefaultImport toImport m@(Module coms mn decls exps)  =
    if isExistingImport `any` decls || mn == toImport then m
    else Module coms mn (ImportDeclaration toImport Implicit Nothing : decls) exps
    where
    isExistingImport (ImportDeclaration mn' _ _) | mn' == toImport = True
    isExistingImport (PositionedDeclaration _ _ d) = isExistingImport d
    isExistingImport _ = False
  
  importPrim :: Module -> Module
  importPrim = addDefaultImport (ModuleName [ProperName C.prim])

-- |
-- A set of make actions that read and write modules from the given directory.
--
buildMakeActions :: FilePath -- ^ the output directory
                 -> (ModuleName -> (Either RebuildPolicy FilePath, Maybe (FilePath, ForeignJS))) -- ^ (rebuild or not, foreign file)
                 -> MakeActions Make
buildMakeActions outputDir getFilePaths =
  MakeActions getInputTimestamp getOutputTimestamp readExterns readForeignModule writeModule progress
  where

  getInputTimestamp :: ModuleName -> Make (Either RebuildPolicy (Maybe UTCTime))
  getInputTimestamp mn = do
    let (path, foreign) = getFilePaths mn
    e1 <- traverseEither getTimestamp path
    fPath <- join <$> traverse (getTimestamp . fst) foreign
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

  readForeignModule :: ModuleName -> Make (Maybe ForeignJS)
  readForeignModule mn = return (snd `fmap` snd (getFilePaths mn))
  
  writeModule :: ModuleName -> String -> String -> Maybe ForeignJS -> Make ()
  writeModule mn js exts foreign = do
    let dir = outputDir </> runModuleName mn
        jsFile      = dir </> "index.js"
        externsFile = dir </> "externs.purs"
        foreignFile = dir </> "foreign.js"
    writeTextFile jsFile js
    writeTextFile externsFile exts
    traverse_ (writeTextFile foreignFile) foreign

  -- Traverse (Either e) instance (base 4.7)
  traverseEither :: Applicative f => (a -> f b) -> Either e a -> f (Either e b)
  traverseEither _ (Left x) = pure (Left x)
  traverseEither f (Right y) = Right <$> f y

  progress :: String -> Make ()
  progress = liftIO . putStrLn
