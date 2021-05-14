module TestUtils where

import Prelude ()
import Prelude.Compat

import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST

import Control.Arrow ((***), (>>>))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Class (tell)
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (sort, sortBy, stripPrefix, groupBy)
import qualified Data.Map as M
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (UTCTime(), diffUTCTime, getCurrentTime, nominalDay)
import Data.Tuple (swap)
import System.Directory
import System.Exit (exitFailure)
import System.Environment (lookupEnv)
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.IO.UTF8 (readUTF8FileT)
import System.Process hiding (cwd)
import qualified System.FilePath.Glob as Glob
import System.IO
import Test.Hspec

findNodeProcess :: IO (Maybe String)
findNodeProcess = runMaybeT . msum $ map (MaybeT . findExecutable) names
  where
  names = ["nodejs", "node"]

-- |
-- Fetches code necessary to run the tests with. The resulting support code
-- should then be checked in, so that npm/bower etc is not required to run the
-- tests.
--
-- Simply rerun this (via ghci is probably easiest) when the support code needs
-- updating.
--
updateSupportCode :: IO ()
updateSupportCode = withCurrentDirectory "tests/support" $ do
  let lastUpdatedFile = ".last_updated"
  skipUpdate <- fmap isJust . runMaybeT $ do
    -- We skip the update if: `.last_updated` exists,
    lastUpdated <- MaybeT $ getModificationTimeMaybe lastUpdatedFile

    -- ... and it was modified less than a day ago (no particular reason why
    -- "one day" specifically),
    now <- lift getCurrentTime
    guard $ now `diffUTCTime` lastUpdated < nominalDay

    -- ... and the needed directories exist,
    contents <- lift $ listDirectory "."
    guard $ "node_modules" `elem` contents && "bower_components" `elem` contents

    -- ... and everything else in `tests/support` is at least as old as
    -- `.last_updated`.
    modTimes <- lift $ traverse getModificationTime . filter (/= lastUpdatedFile) $ contents
    guard $ all (<= lastUpdated) modTimes

    pure ()

  unless skipUpdate $ do
    heading "Updating support code"
    callCommand "npm install"
    -- bower uses shebang "/usr/bin/env node", but we might have nodejs
    node <- maybe cannotFindNode pure =<< findNodeProcess
    -- Sometimes we run as a root (e.g. in simple docker containers)
    -- And we are non-interactive: https://github.com/bower/bower/issues/1162
    callProcess node ["node_modules/bower/bin/bower", "--allow-root", "install", "--config.interactive=false"]
    writeFile lastUpdatedFile ""
  where
  cannotFindNode :: IO a
  cannotFindNode = do
    hPutStrLn stderr "Cannot find node (or nodejs) executable"
    exitFailure

  getModificationTimeMaybe :: FilePath -> IO (Maybe UTCTime)
  getModificationTimeMaybe f = catch (Just <$> getModificationTime f) $ \case
    e | isDoesNotExistError e -> pure Nothing
      | otherwise             -> throw e

  heading msg = do
    putStrLn ""
    putStrLn $ replicate 79 '#'
    putStrLn $ "# " ++ msg
    putStrLn $ replicate 79 '#'
    putStrLn ""

readInput :: [FilePath] -> IO [(FilePath, T.Text)]
readInput inputFiles = forM inputFiles $ \inputFile -> do
  text <- readUTF8FileT inputFile
  return (inputFile, text)

-- |
-- The support modules that should be cached between test cases, to avoid
-- excessive rebuilding.
--
getSupportModuleTuples :: IO [(FilePath, P.Module)]
getSupportModuleTuples = do
  cd <- getCurrentDirectory
  let supportDir = cd </> "tests" </> "support"
  psciFiles <- Glob.globDir1 (Glob.compile "**/*.purs") (supportDir </> "psci")
  libraries <- Glob.globDir1 (Glob.compile "purescript-*/src/**/*.purs") (supportDir </> "bower_components")
  let pursFiles = psciFiles ++ libraries
  fileContents <- readInput pursFiles
  modules <- runExceptT $ ExceptT . return $ CST.parseFromFiles id fileContents
  case modules of
    Right ms -> return (fmap (fmap snd) ms)
    Left errs -> fail (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)

getSupportModuleNames :: IO [T.Text]
getSupportModuleNames = sort . map (P.runModuleName . P.getModuleName . snd) <$> getSupportModuleTuples

pushd :: forall a. FilePath -> IO a -> IO a
pushd dir act = do
  original <- getCurrentDirectory
  setCurrentDirectory dir
  result <- try act :: IO (Either IOException a)
  setCurrentDirectory original
  either throwIO return result


createOutputFile :: FilePath -> IO Handle
createOutputFile logfileName = do
  tmp <- getTemporaryDirectory
  createDirectoryIfMissing False (tmp </> logpath)
  openFile (tmp </> logpath </> logfileName) WriteMode

data SupportModules = SupportModules
  { supportModules :: [P.Module]
  , supportExterns :: [P.ExternsFile]
  , supportForeigns :: M.Map P.ModuleName FilePath
  }

setupSupportModules :: IO SupportModules
setupSupportModules = do
  ms <- getSupportModuleTuples
  let modules = map snd ms
  supportExterns <- runExceptT $ do
    foreigns <- inferForeignModules ms
    externs <- ExceptT . fmap fst . runTest $ P.make (makeActions modules foreigns) (CST.pureResult <$> modules)
    return (externs, foreigns)
  case supportExterns of
    Left errs -> fail (P.prettyPrintMultipleErrors P.defaultPPEOptions errs)
    Right (externs, foreigns) -> return $ SupportModules modules externs foreigns

getTestFiles :: FilePath -> IO [[FilePath]]
getTestFiles testDir = do
  cwd <- getCurrentDirectory
  let dir = "tests" </> "purs" </> testDir
  testsInPath <- getFiles dir <$> testGlob dir
  let rerunPath = dir </> "RerunCompilerTests.txt"
  hasRerunFile <- doesFileExist rerunPath
  rerunTests <-
    if hasRerunFile
    then let compilerTestDir = cwd </> "tests" </> "purs" </> "passing"
             textToTestFiles
               = mapM (\path -> ((path ++ ".purs") :) <$> testGlob path)
               . map ((compilerTestDir </>) . T.unpack)
               . filter (not . T.null)
               . map (T.strip . fst . T.breakOn "--")
               . T.lines
         in readUTF8FileT rerunPath >>= textToTestFiles
    else return []
  return $ testsInPath ++ rerunTests
  where
  -- A glob for all purs and js files within a test directory
  testGlob :: FilePath -> IO [FilePath]
  testGlob = Glob.globDir1 (Glob.compile "**/*.purs")
  -- Groups the test files so that a top-level file can have dependencies in a
  -- subdirectory of the same name. The inner tuple contains a list of the
  -- .purs files and the .js files for the test case.
  getFiles :: FilePath -> [FilePath] -> [[FilePath]]
  getFiles baseDir
    = map (filter ((== ".purs") . takeExtensions) . map (baseDir </>))
    . groupBy ((==) `on` extractPrefix)
    . sortBy (compare `on` extractPrefix)
    . map (makeRelative baseDir)
  -- Extracts the filename part of a .purs file, or if the file is in a
  -- subdirectory, the first part of that directory path.
  extractPrefix :: FilePath -> FilePath
  extractPrefix fp =
    let dir = takeDirectory fp
        ext = reverse ".purs"
    in if dir == "."
       then maybe fp reverse $ stripPrefix ext $ reverse fp
       else dir

compile
  :: SupportModules
  -> [FilePath]
  -> IO (Either P.MultipleErrors [P.ExternsFile], P.MultipleErrors)
compile SupportModules{..} inputFiles = runTest $ do
  -- Sorting the input files makes some messages (e.g., duplicate module) deterministic
  fs <- liftIO $ readInput (sort inputFiles)
  msWithWarnings <- CST.parseFromFiles id fs
  tell $ foldMap (\(fp, (ws, _)) -> CST.toMultipleWarnings fp ws) msWithWarnings
  let ms = fmap snd <$> msWithWarnings
  foreigns <- inferForeignModules ms
  let actions = makeActions supportModules (foreigns `M.union` supportForeigns)
  case ms of
    [singleModule] -> pure <$> P.rebuildModule actions supportExterns (snd singleModule)
    _ -> P.make actions (CST.pureResult <$> supportModules ++ map snd ms)

makeActions :: [P.Module] -> M.Map P.ModuleName FilePath -> P.MakeActions P.Make
makeActions modules foreigns = (P.buildMakeActions modulesDir (P.internalError "makeActions: input file map was read.") foreigns False)
                               { P.getInputTimestampsAndHashes = getInputTimestampsAndHashes
                               , P.getOutputTimestamp = getOutputTimestamp
                               , P.progress = const (pure ())
                               }
  where
  getInputTimestampsAndHashes :: P.ModuleName -> P.Make (Either P.RebuildPolicy a)
  getInputTimestampsAndHashes mn
    | isSupportModule (P.runModuleName mn) = return (Left P.RebuildNever)
    | otherwise = return (Left P.RebuildAlways)
    where
    isSupportModule = flip elem (map (P.runModuleName . P.getModuleName) modules)

  getOutputTimestamp :: P.ModuleName -> P.Make (Maybe UTCTime)
  getOutputTimestamp mn = do
    let filePath = modulesDir </> T.unpack (P.runModuleName mn)
    exists <- liftIO $ doesDirectoryExist filePath
    return (if exists then Just (P.internalError "getOutputTimestamp: read timestamp") else Nothing)


runTest :: P.Make a -> IO (Either P.MultipleErrors a, P.MultipleErrors)
runTest = P.runMake P.defaultOptions

inferForeignModules
  :: MonadIO m
  => [(FilePath, P.Module)]
  -> m (M.Map P.ModuleName FilePath)
inferForeignModules = P.inferForeignModules . fromList
  where
    fromList :: [(FilePath, P.Module)] -> M.Map P.ModuleName (Either P.RebuildPolicy FilePath)
    fromList = M.fromList . map ((P.getModuleName *** Right) . swap)

trim :: String -> String
trim = dropWhile isSpace >>> reverse >>> dropWhile isSpace >>> reverse

modulesDir :: FilePath
modulesDir = ".test_modules" </> "node_modules"

logpath :: FilePath
logpath = "purescript-output"

-- | Assert that the contents of the provided file path match the result of the
-- provided action. If the "HSPEC_ACCEPT" environment variable is set, or if the
-- file does not already exist, we write the resulting ByteString out to the
-- provided file path instead. However, if the "CI" environment variable is
-- set, "HSPEC_ACCEPT" is ignored and we require that the file does exist with
-- the correct contents (see #3808). Based (very loosely) on the tasty-golden
-- package.
goldenVsString
  :: HasCallStack -- For expectationFailure; use the call site for better failure locations
  => FilePath
  -> IO ByteString
  -> Expectation
goldenVsString goldenFile testAction = do
  accept <- isJust <$> lookupEnv "HSPEC_ACCEPT"
  ci <- isJust <$> lookupEnv "CI"
  goldenContents <- tryJust (guard . isDoesNotExistError) (BS.readFile goldenFile)
  case goldenContents of
    Left () ->
      -- The golden file does not exist
      if ci
        then expectationFailure $ "Missing golden file: " ++ goldenFile
        else createOrReplaceGoldenFile

    Right _ | not ci && accept ->
      createOrReplaceGoldenFile

    Right expected -> do
      actual <- testAction
      if expected == actual
        then pure ()
        else expectationFailure $
          "Test output differed from '" ++ goldenFile ++ "'; got:\n" ++
          T.unpack (T.decodeUtf8With (\_ _ -> Just '\xFFFD') actual)
  where
  createOrReplaceGoldenFile = do
    testAction >>= BS.writeFile goldenFile
    pendingWith "Accepting new output"
