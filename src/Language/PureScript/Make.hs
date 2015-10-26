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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
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
import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.Supply
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))

import Control.Concurrent.Lifted as C

import Data.List (foldl', sort)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Time.Clock
import Data.String (fromString)
import Data.Foldable (for_)
#if __GLASGOW_HASKELL__ < 710
import Data.Traversable (traverse)
#endif
import Data.Traversable (for)
import Data.Version (showVersion)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import qualified Data.Map as M

import System.Directory
       (doesFileExist, getModificationTime, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO.Error (tryIOError)

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Externs
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Linter
import Language.PureScript.ModuleDependencies
import Language.PureScript.Names
import Language.PureScript.Options
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
  , readExterns :: ModuleName -> m (FilePath, B.ByteString)
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
type Externs = B.ByteString

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
make :: forall m. (Functor m, Applicative m, Monad m, MonadBaseControl IO m, MonadReader Options m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [Module]
     -> m Environment
make MakeActions{..} ms = do
  checkModuleNamesAreUnique

  (sorted, graph) <- sortModules ms

  barriers <- zip (map getModuleName sorted) <$> replicateM (length ms) ((,) <$> C.newEmptyMVar <*> C.newEmptyMVar)

  for_ sorted $ \m -> fork $ do
    let deps = fromMaybe (internalError "make: module not found in dependency graph.") (lookup (getModuleName m) graph)
    buildModule barriers (importPrim m) (deps `inOrderOf` map getModuleName sorted)

  -- Wait for all threads to complete, and collect errors.
  errors <- catMaybes <$> for barriers (takeMVar . snd . snd)

  -- All threads have completed, rethrow any caught errors.
  unless (null errors) $ throwError (mconcat errors)

  -- Bundle up all the externs and return them as an Environment
  (_, externs) <- unzip . fromMaybe (internalError "make: externs were missing but no errors reported.") . sequence <$> for barriers (takeMVar . fst . snd)
  return $ foldl' (flip applyExternsFileToEnvironment) initEnvironment externs

  where
  checkModuleNamesAreUnique :: m ()
  checkModuleNamesAreUnique =
    case findDuplicate (map getModuleName ms) of
      Nothing -> return ()
      Just mn -> throwError . errorMessage $ DuplicateModuleName mn

  -- Verify that a list of values has unique keys
  findDuplicate :: (Ord a) => [a] -> Maybe a
  findDuplicate = go . sort
    where
    go (x : y : xs)
      | x == y = Just x
      | otherwise = go (y : xs)
    go _ = Nothing

  -- Sort a list so its elements appear in the same order as in another list.
  inOrderOf :: (Ord a) => [a] -> [a] -> [a]
  inOrderOf xs ys = let s = S.fromList xs in filter (`S.member` s) ys

  buildModule :: [(ModuleName, (C.MVar (Maybe (MultipleErrors, ExternsFile)), C.MVar (Maybe MultipleErrors)))] -> Module -> [ModuleName] -> m ()
  buildModule barriers m@(Module _ _ moduleName _ _) deps = flip catchError (markComplete Nothing . Just) $ do
    -- We need to wait for dependencies to be built, before checking if the current
    -- module should be rebuilt, so the first thing to do is to wait on the
    -- MVars for the module's dependencies.
    mexterns <- fmap unzip . sequence <$> mapM (readMVar . fst . fromMaybe (internalError "make: no barrier") . flip lookup barriers) deps

    case mexterns of
      Just (_, externs) -> do
        outputTimestamp <- getOutputTimestamp moduleName
        dependencyTimestamp <- maximumMaybe <$> mapM (fmap shouldExist . getOutputTimestamp) deps
        inputTimestamp <- getInputTimestamp moduleName

        let shouldRebuild = case (inputTimestamp, dependencyTimestamp, outputTimestamp) of
                              (Right (Just t1), Just t3, Just t2) -> t1 > t2 || t3 > t2
                              (Right (Just t1), Nothing, Just t2) -> t1 > t2
                              (Left RebuildNever, _, Just _) -> False
                              _ -> True

        let rebuild = do
              (exts, warnings) <- listen $ do
                progress $ CompilingModule moduleName
                let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
                lint m
                ([desugared], nextVar) <- runSupplyT 0 $ desugar externs [m]
                (checked@(Module ss coms _ elaborated exps), env') <- runCheck' env $ typeCheckModule desugared
                checkExhaustiveModule env' checked
                regrouped <- createBindingGroups moduleName . collapseBindingGroups $ elaborated
                let mod' = Module ss coms moduleName regrouped exps
                    corefn = CF.moduleToCoreFn env' mod'
                    [renamed] = renameInModules [corefn]
                    exts = moduleToExternsFile mod' env'
                evalSupplyT nextVar $ codegen renamed env' $ encode exts
                return exts
              markComplete (Just (warnings, exts)) Nothing

        if shouldRebuild
          then rebuild
          else do
            mexts <- decodeExterns . snd <$> readExterns moduleName
            case mexts of
              Just exts -> markComplete (Just (mempty, exts)) Nothing
              Nothing -> rebuild
      Nothing -> markComplete Nothing Nothing
    where
    markComplete :: Maybe (MultipleErrors, ExternsFile) -> Maybe MultipleErrors -> m ()
    markComplete externs errors = do
      putMVar (fst $ fromMaybe (internalError "make: no barrier") $ lookup moduleName barriers) externs
      putMVar (snd $ fromMaybe (internalError "make: no barrier") $ lookup moduleName barriers) errors

  maximumMaybe :: (Ord a) => [a] -> Maybe a
  maximumMaybe [] = Nothing
  maximumMaybe xs = Just $ maximum xs

  -- Make sure a dependency exists
  shouldExist :: Maybe UTCTime -> UTCTime
  shouldExist (Just t) = t
  shouldExist _ = internalError "make: dependency should already have been built."

  decodeExterns :: B.ByteString -> Maybe ExternsFile
  decodeExterns bs = do
    externs <- decode bs
    guard $ efVersion externs == showVersion Paths.version
    return externs

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
newtype Make a = Make { unMake :: ReaderT Options (ExceptT MultipleErrors (Logger MultipleErrors)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError MultipleErrors, MonadWriter MultipleErrors, MonadReader Options)

instance MonadBase IO Make where
  liftBase = liftIO

instance MonadBaseControl IO Make where
  type StM Make a = Either MultipleErrors a
  liftBaseWith f = Make $ liftBaseWith $ \q -> f (q . unMake)
  restoreM = Make . restoreM

-- |
-- Execute a 'Make' monad, returning either errors, or the result of the compile plus any warnings.
--
runMake :: Options -> Make a -> IO (Either MultipleErrors a, MultipleErrors)
runMake opts = runLogger' . runExceptT . flip runReaderT opts . unMake

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
    let path = fromMaybe (internalError "Module has no filename in 'make'") $ M.lookup mn filePathMap
    e1 <- traverseEither getTimestamp path
    fPath <- maybe (return Nothing) getTimestamp $ M.lookup mn foreigns
    return $ fmap (max fPath) e1

  getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    let filePath = runModuleName mn
        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.json"
    min <$> getTimestamp jsFile <*> getTimestamp externsFile

  readExterns :: ModuleName -> Make (FilePath, B.ByteString)
  readExterns mn = do
    let path = outputDir </> runModuleName mn </> "externs.json"
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
        externsFile = outputDir </> filePath </> "externs.json"
        foreignFile = outputDir </> filePath </> "foreign.js"
        prefix = ["Generated by psc version " ++ showVersion Paths.version | usePrefix]
        js = unlines $ map ("// " ++) prefix ++ [pjs]
    lift $ do
      writeTextFile jsFile (fromString js)
      for_ (mn `M.lookup` foreigns) (readTextFile >=> writeTextFile foreignFile)
      writeTextFile externsFile exts

  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign

  getTimestamp :: FilePath -> Make (Maybe UTCTime)
  getTimestamp path = makeIO (const (ErrorMessage [] $ CannotGetFileInfo path)) $ do
    exists <- doesFileExist path
    traverse (const $ getModificationTime path) $ guard exists

  readTextFile :: FilePath -> Make B.ByteString
  readTextFile path = makeIO (const (ErrorMessage [] $ CannotReadFile path)) $ B.readFile path

  writeTextFile :: FilePath -> B.ByteString -> Make ()
  writeTextFile path text = makeIO (const (ErrorMessage [] $ CannotWriteFile path)) $ do
    mkdirp path
    B.writeFile path text
    where
    mkdirp :: FilePath -> IO ()
    mkdirp = createDirectoryIfMissing True . takeDirectory

  progress :: ProgressMessage -> Make ()
  progress = liftIO . putStrLn . renderProgressMessage
