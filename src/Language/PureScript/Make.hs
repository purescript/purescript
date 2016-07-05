{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.PureScript.Make
  (
  -- * Make API
    RebuildPolicy(..)
  , ProgressMessage(..), renderProgressMessage
  , MakeActions(..)
  , Externs()
  , rebuildModule
  , make

  -- * Implementation of Make API using files on disk
  , Make(..)
  , runMake
  , makeIO
  , readTextFile
  , buildMakeActions
  , inferForeignModules
  ) where

import Prelude.Compat

import Control.Concurrent.Lifted as C
import Control.Monad hiding (sequence)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader (MonadReader(..), ReaderT(..), asks)
import Control.Monad.Supply
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Except
import Control.Monad.Writer.Class (MonadWriter(..))

import Data.Aeson (encode, decode)
import Data.Either (partitionEithers)
import Data.Foldable (for_)
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe, catMaybes)
import Data.String (fromString)
import Data.Time.Clock
import Data.Traversable (for)
import Data.Version (showVersion)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.UTF8 as BU8
import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Externs
import Language.PureScript.Linter
import Language.PureScript.ModuleDependencies
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.Pretty
import Language.PureScript.Pretty.Common(SMap(..))
import Language.PureScript.Renamer
import Language.PureScript.Sugar
import Language.PureScript.TypeChecker
import qualified Language.JavaScript.Parser as JS
import qualified Language.PureScript.Bundle as Bundle
import qualified Language.PureScript.CodeGen.JS as J
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.Parser as PSParser

import qualified Paths_purescript as Paths

import SourceMap
import SourceMap.Types

import System.Directory (doesFileExist, getModificationTime, createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>), takeDirectory, makeRelative, splitPath, normalise, replaceExtension)
import System.IO.Error (tryIOError)
import System.IO.UTF8 (readUTF8File, writeUTF8File)

import qualified Text.Parsec as Parsec

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
data MakeActions m = MakeActions
  { getInputTimestamp :: ModuleName -> m (Either RebuildPolicy (Maybe UTCTime))
  -- ^ Get the timestamp for the input file(s) for a module. If there are multiple
  -- files (@.purs@ and foreign files, for example) the timestamp should be for
  -- the most recently modified file.
  , getOutputTimestamp :: ModuleName -> m (Maybe UTCTime)
  -- ^ Get the timestamp for the output files for a module. This should be the
  -- timestamp for the oldest modified file, or 'Nothing' if any of the required
  -- output files are missing.
  , readExterns :: ModuleName -> m (FilePath, Externs)
  -- ^ Read the externs file for a module as a string and also return the actual
  -- path for the file.
  , codegen :: CF.Module CF.Ann -> Environment -> Externs -> SupplyT m ()
  -- ^ Run the code generator for the module and write any required output files.
  , progress :: ProgressMessage -> m ()
  -- ^ Respond to a progress update.
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

-- | Rebuild a single module
rebuildModule :: forall m. (Monad m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [ExternsFile]
     -> Module
     -> m ExternsFile
rebuildModule MakeActions{..} externs m@(Module _ _ moduleName _ _) = do
  progress $ CompilingModule moduleName
  let env = foldl' (flip applyExternsFileToEnvironment) initEnvironment externs
      withPrim = importPrim m
  lint withPrim
  ((Module ss coms _ elaborated exps, env'), nextVar) <- runSupplyT 0 $ do
    [desugared] <- desugar externs [withPrim]
    runCheck' env $ typeCheckModule desugared
  regrouped <- createBindingGroups moduleName . collapseBindingGroups $ elaborated
  let mod' = Module ss coms moduleName regrouped exps
      corefn = CF.moduleToCoreFn env' mod'
      [renamed] = renameInModules [corefn]
      exts = moduleToExternsFile mod' env'
  evalSupplyT nextVar . codegen renamed env' . BU8.toString . B.toStrict . encode $ exts
  return exts

-- |
-- Compiles in "make" mode, compiling each module separately to a js files and an externs file
--
-- If timestamps have not changed, the externs file can be used to provide the module's types without
-- having to typecheck the module again.
--
make :: forall m. (Monad m, MonadBaseControl IO m, MonadError MultipleErrors m, MonadWriter MultipleErrors m)
     => MakeActions m
     -> [Module]
     -> m [ExternsFile]
make ma@MakeActions{..} ms = do
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
  return externs

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
    mexterns <- fmap unzip . sequence <$> traverse (readMVar . fst . fromMaybe (internalError "make: no barrier") . flip lookup barriers) deps

    case mexterns of
      Just (_, externs) -> do
        outputTimestamp <- getOutputTimestamp moduleName
        dependencyTimestamp <- maximumMaybe <$> traverse (fmap shouldExist . getOutputTimestamp) deps
        inputTimestamp <- getInputTimestamp moduleName

        let shouldRebuild = case (inputTimestamp, dependencyTimestamp, outputTimestamp) of
                              (Right (Just t1), Just t3, Just t2) -> t1 > t2 || t3 > t2
                              (Right (Just t1), Nothing, Just t2) -> t1 > t2
                              (Left RebuildNever, _, Just _) -> False
                              _ -> True

        let rebuild = do
              (exts, warnings) <- listen $ rebuildModule ma externs m
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

  decodeExterns :: Externs -> Maybe ExternsFile
  decodeExterns bs = do
    externs <- decode (fromString bs)
    guard $ efVersion externs == showVersion Paths.version
    return externs

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

-- | Read a text file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
readTextFile :: FilePath -> Make String
readTextFile path = makeIO (const (ErrorMessage [] $ CannotReadFile path)) $ readUTF8File path

-- | Infer the module name for a module by looking for the same filename with
-- a .js extension.
inferForeignModules
  :: forall m
   . MonadIO m
  => M.Map ModuleName (Either RebuildPolicy FilePath)
  -> m (M.Map ModuleName FilePath)
inferForeignModules = fmap (M.mapMaybe id) . traverse inferForeignModule
  where
    inferForeignModule :: Either RebuildPolicy FilePath -> m (Maybe FilePath)
    inferForeignModule (Left _) = return Nothing
    inferForeignModule (Right path) = do
      let jsFile = replaceExtension path "js"
      exists <- liftIO $ doesFileExist jsFile
      if exists
        then return (Just jsFile)
        else return Nothing

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
    e1 <- traverse getTimestamp path
    fPath <- maybe (return Nothing) getTimestamp $ M.lookup mn foreigns
    return $ fmap (max fPath) e1

  getOutputTimestamp :: ModuleName -> Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    let filePath = runModuleName mn
        jsFile = outputDir </> filePath </> "index.js"
        externsFile = outputDir </> filePath </> "externs.json"
    min <$> getTimestamp jsFile <*> getTimestamp externsFile

  readExterns :: ModuleName -> Make (FilePath, Externs)
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
        | otherwise -> do
            checkForeignDecls m path
            return $ Just $ J.JSApp Nothing (J.JSVar Nothing "require") [J.JSStringLiteral Nothing "./foreign"]
      Nothing | requiresForeign m -> throwError . errorMessage $ MissingFFIModule mn
              | otherwise -> return Nothing
    rawJs <- J.moduleToJs m foreignInclude
    dir <- lift $ makeIO (const (ErrorMessage [] $ CannotGetFileInfo ".")) getCurrentDirectory
    sourceMaps <- lift $ asks optionsSourceMaps
    let (pjs, mappings) = if sourceMaps then prettyPrintJSWithSourceMaps rawJs else (prettyPrintJS rawJs, [])
    let filePath = runModuleName mn
        jsFile = outputDir </> filePath </> "index.js"
        mapFile = outputDir </> filePath </> "index.js.map"
        externsFile = outputDir </> filePath </> "externs.json"
        foreignFile = outputDir </> filePath </> "foreign.js"
        prefix = ["Generated by psc version " ++ showVersion Paths.version | usePrefix]
        js = unlines $ map ("// " ++) prefix ++ [pjs]
        mapRef = if sourceMaps then "//# sourceMappingURL=index.js.map\n" else ""
    lift $ do
      writeTextFile jsFile (fromString $ js ++ mapRef)
      for_ (mn `M.lookup` foreigns) (readTextFile >=> writeTextFile foreignFile)
      writeTextFile externsFile exts
    lift $ when sourceMaps $ genSourceMap dir mapFile (length prefix) mappings

  genSourceMap :: String -> String -> Int -> [SMap] -> Make ()
  genSourceMap dir mapFile extraLines mappings = do
    let pathToDir = iterate (".." </>) ".." !! length (splitPath $ normalise outputDir)
        sourceFile = case mappings of
                      (SMap file _ _ : _) -> Just $ pathToDir </> makeRelative dir file
                      _ -> Nothing
    let rawMapping = SourceMapping { smFile = "index.js", smSourceRoot = Nothing, smMappings =
      map (\(SMap _ orig gen) -> Mapping {
          mapOriginal = Just $ convertPos $ add 0 (-1) orig
        , mapSourceFile = sourceFile
        , mapGenerated = convertPos $ add (extraLines+1) 0 gen
        , mapName = Nothing
        }) mappings
    }
    let mapping = generate rawMapping
    writeTextFile mapFile $ BU8.toString . B.toStrict . encode $ mapping
    where
    add :: Int -> Int -> SourcePos -> SourcePos
    add n m (SourcePos n' m') = SourcePos (n+n') (m+m')

    convertPos :: SourcePos -> Pos
    convertPos SourcePos { sourcePosLine = l, sourcePosColumn = c } =
      Pos { posLine = fromIntegral l, posColumn = fromIntegral c }

  requiresForeign :: CF.Module a -> Bool
  requiresForeign = not . null . CF.moduleForeign

  getTimestamp :: FilePath -> Make (Maybe UTCTime)
  getTimestamp path = makeIO (const (ErrorMessage [] $ CannotGetFileInfo path)) $ do
    exists <- doesFileExist path
    traverse (const $ getModificationTime path) $ guard exists

  writeTextFile :: FilePath -> String -> Make ()
  writeTextFile path text = makeIO (const (ErrorMessage [] $ CannotWriteFile path)) $ do
    mkdirp path
    writeUTF8File path text
    where
    mkdirp :: FilePath -> IO ()
    mkdirp = createDirectoryIfMissing True . takeDirectory

  progress :: ProgressMessage -> Make ()
  progress = liftIO . putStrLn . renderProgressMessage

-- |
-- Check that the declarations in a given PureScript module match with those
-- in its corresponding foreign module.
--
checkForeignDecls :: CF.Module ann -> FilePath -> SupplyT Make ()
checkForeignDecls m path = do
  jsStr <- lift $ readTextFile path
  js <- either (errorParsingModule . Bundle.UnableToParseModule) pure $ JS.parse jsStr path

  foreignIdentsStrs <- either errorParsingModule pure $ getExps js
  foreignIdents <- either
                     errorInvalidForeignIdentifiers
                     (pure . S.fromList)
                     (parseIdents foreignIdentsStrs)
  let importedIdents = S.fromList $ map fst (CF.moduleForeign m)

  let unusedFFI = foreignIdents S.\\ importedIdents
  unless (null unusedFFI) $
    tell . errorMessage . UnusedFFIImplementations mname $
      S.toList unusedFFI

  let missingFFI = importedIdents S.\\ foreignIdents
  unless (null missingFFI) $
    throwError . errorMessage . MissingFFIImplementations mname $
      S.toList missingFFI

  where
  mname = CF.moduleName m

  errorParsingModule :: Bundle.ErrorMessage -> SupplyT Make a
  errorParsingModule = throwError . errorMessage . ErrorParsingFFIModule path . Just

  getExps :: JS.JSAST -> Either Bundle.ErrorMessage [String]
  getExps = Bundle.getExportedIdentifiers (runModuleName mname)

  errorInvalidForeignIdentifiers :: [String] -> SupplyT Make a
  errorInvalidForeignIdentifiers =
    throwError . mconcat . map (errorMessage . InvalidFFIIdentifier mname)

  parseIdents :: [String] -> Either [String] [Ident]
  parseIdents strs =
    case partitionEithers (map parseIdent strs) of
      ([], idents) ->
        Right idents
      (errs, _) ->
        Left errs

  -- We ignore the error message here, just being told it's an invalid
  -- identifier should be enough.
  parseIdent :: String -> Either String Ident
  parseIdent str = try str
    where
    try s = either (const (Left str)) Right $ do
      ts <- PSParser.lex "" s
      PSParser.runTokenParser "" (PSParser.parseIdent <* Parsec.eof) ts
