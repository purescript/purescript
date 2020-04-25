{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.Publish
  ( preparePackage
  , preparePackage'
  , unsafePreparePackage
  , PrepareM()
  , runPrepareM
  , warn
  , userError
  , internalError
  , otherError
  , PublishOptions(..)
  , defaultPublishOptions
  , getGitWorkingTreeStatus
  , checkCleanWorkingTree
  , getVersionFromGitTag
  , getManifestRepositoryInfo
  , getModules
  ) where

import Protolude hiding (stdin)

import Control.Arrow ((***))
import Control.Category ((>>>))
import Control.Monad.Writer.Strict (MonadWriter, WriterT, runWriterT, tell)

import Data.Aeson.BetterErrors (Parse, parse, keyMay, eachInObjectWithKey, key, asString, withString)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.String (String, lines)
import Data.List (stripPrefix, (\\))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Version
import qualified Distribution.SPDX as SPDX
import qualified Distribution.Parsec.Class as CabalParsec

import System.Directory (doesFileExist)
import System.FilePath.Glob (globDir1)
import System.Process (readProcess)

import Web.Bower.PackageMeta (PackageMeta(..), PackageName, Repository(..))
import qualified Web.Bower.PackageMeta as Bower

import Language.PureScript.Publish.ErrorsWarnings
import Language.PureScript.Publish.Utils
import qualified Language.PureScript as P (version, ModuleName)
import qualified Language.PureScript.Docs as D

data PublishOptions = PublishOptions
  { -- | How to obtain the version tag and version that the data being
    -- generated will refer to.
    publishGetVersion :: PrepareM (Text, Version)
  , publishGetTagTime :: Text -> PrepareM UTCTime
  , -- | What to do when the working tree is dirty
    publishWorkingTreeDirty :: PrepareM ()
  , -- | Compiler output directory (which must include up-to-date docs.json
    -- files for any modules we are producing docs for).
    publishCompileOutputDir :: FilePath
  , -- | Path to the manifest file; a JSON file including information about the
    -- package, such as name, author, dependency version bounds.
    publishManifestFile :: FilePath
  , -- | Path to the resolutions file; a JSON file containing all of the
    -- package's dependencies, their versions, and their paths on the disk.
    publishResolutionsFile :: FilePath
  }

defaultPublishOptions :: PublishOptions
defaultPublishOptions = PublishOptions
  { publishGetVersion = getVersionFromGitTag
  , publishGetTagTime = getTagTime
  , publishWorkingTreeDirty = userError DirtyWorkingTree
  , publishCompileOutputDir = "output"
  , publishManifestFile = "bower.json"
  , publishResolutionsFile = "resolutions.json"
  }

-- | Attempt to retrieve package metadata from the current directory.
-- Calls exitFailure if no package metadata could be retrieved.
unsafePreparePackage :: PublishOptions -> IO D.UploadedPackage
unsafePreparePackage opts =
  either (\e -> printError e >> exitFailure) pure
    =<< preparePackage opts

-- | Attempt to retrieve package metadata from the current directory.
-- Returns a PackageError on failure
preparePackage :: PublishOptions -> IO (Either PackageError D.UploadedPackage)
preparePackage opts =
  runPrepareM (preparePackage' opts)
    >>= either (pure . Left) (fmap Right . handleWarnings)

  where
  handleWarnings (result, warns) = do
    printWarnings warns
    return result

newtype PrepareM a =
  PrepareM { unPrepareM :: WriterT [PackageWarning] (ExceptT PackageError IO) a }
  deriving (Functor, Applicative, Monad,
            MonadWriter [PackageWarning],
            MonadError PackageError)

-- This MonadIO instance ensures that IO errors don't crash the program.
instance MonadIO PrepareM where
  liftIO act =
    lift' (try act) >>= either (otherError . IOExceptionThrown) return
    where
    lift' :: IO a -> PrepareM a
    lift' = PrepareM . lift . lift

runPrepareM :: PrepareM a -> IO (Either PackageError (a, [PackageWarning]))
runPrepareM = runExceptT . runWriterT . unPrepareM

warn :: PackageWarning -> PrepareM ()
warn w = tell [w]

userError :: UserError -> PrepareM a
userError = throwError . UserError

internalError :: InternalError -> PrepareM a
internalError = throwError . InternalError

otherError :: OtherError -> PrepareM a
otherError = throwError . OtherError

catchLeft :: Applicative f => Either a b -> (a -> f b) -> f b
catchLeft a f = either f pure a

preparePackage' :: PublishOptions -> PrepareM D.UploadedPackage
preparePackage' opts = do
  unlessM (liftIO (doesFileExist (publishManifestFile opts))) (userError PackageManifestNotFound)
  checkCleanWorkingTree opts

  pkgMeta <- liftIO (Bower.decodeFile (publishManifestFile opts))
               >>= flip catchLeft (userError . CouldntDecodePackageManifest)
  checkLicense pkgMeta

  (pkgVersionTag, pkgVersion) <- publishGetVersion opts
  pkgTagTime <- Just <$> publishGetTagTime opts pkgVersionTag
  pkgGithub <- getManifestRepositoryInfo pkgMeta

  resolvedDeps <- parseResolutionsFile (publishResolutionsFile opts)

  (pkgModules, pkgModuleMap)  <- getModules opts (map (second fst) resolvedDeps)

  let declaredDeps = map fst $
                       Bower.bowerDependencies pkgMeta
                       ++ Bower.bowerDevDependencies pkgMeta
  pkgResolvedDependencies <- handleDeps declaredDeps (map (second snd) resolvedDeps)

  let pkgUploader = D.NotYetKnown
  let pkgCompilerVersion = P.version


  return D.Package{..}

getModules
  :: PublishOptions
  -> [(PackageName, FilePath)]
  -> PrepareM ([D.Module], Map P.ModuleName PackageName)
getModules opts paths = do
  (inputFiles, depsFiles) <- liftIO (getInputAndDepsFiles paths)

  (modules, moduleMap) <-
    (liftIO (runExceptT (D.collectDocs (publishCompileOutputDir opts) inputFiles depsFiles)))
    >>= either (userError . CompileError) return

  pure (map snd modules, moduleMap)

data TreeStatus = Clean | Dirty deriving (Show, Eq, Ord, Enum)

getGitWorkingTreeStatus :: PrepareM TreeStatus
getGitWorkingTreeStatus = do
  out <- readProcess' "git" ["status", "--porcelain"] ""
  return $
    if all null . lines $ out
      then Clean
      else Dirty

checkCleanWorkingTree :: PublishOptions -> PrepareM ()
checkCleanWorkingTree opts = do
  status <- getGitWorkingTreeStatus
  unless (status == Clean) $
    publishWorkingTreeDirty opts

getVersionFromGitTag :: PrepareM (Text, Version)
getVersionFromGitTag = do
  out <- readProcess' "git" ["tag", "--list", "--points-at", "HEAD"] ""
  let vs = map trimWhitespace (lines out)
  case mapMaybe parseMay vs of
    []  -> userError TagMustBeCheckedOut
    [x] -> return (first T.pack x)
    xs  -> userError (AmbiguousVersions (map snd xs))
  where
  trimWhitespace =
    dropWhile isSpace >>> reverse >>> dropWhile isSpace >>> reverse
  parseMay str = do
    digits <- stripPrefix "v" str
    (str,) <$> D.parseVersion' digits

-- | Given a git tag, get the time it was created.
getTagTime :: Text -> PrepareM UTCTime
getTagTime tag = do
  out <- readProcess' "git" ["log", "-1", "--format=%ct", T.unpack tag] ""
  case mapMaybe readMaybe (lines out) of
    [t] -> pure . posixSecondsToUTCTime . fromInteger $ t
    _ -> internalError (CouldntParseGitTagDate tag)

getManifestRepositoryInfo :: PackageMeta -> PrepareM (D.GithubUser, D.GithubRepo)
getManifestRepositoryInfo pkgMeta =
  case bowerRepository pkgMeta of
    Nothing -> do
      giturl <- catchError (Just . T.strip . T.pack <$> readProcess' "git" ["config", "remote.origin.url"] "")
                  (const (return Nothing))
      userError (BadRepositoryField (RepositoryFieldMissing (giturl >>= extractGithub >>= return . format)))
    Just Repository{..} -> do
      unless (repositoryType == "git")
        (userError (BadRepositoryField (BadRepositoryType repositoryType)))
      maybe (userError (BadRepositoryField NotOnGithub)) return (extractGithub repositoryUrl)

  where
    format :: (D.GithubUser, D.GithubRepo) -> Text
    format (user, repo) = "https://github.com/" <> D.runGithubUser user <> "/" <> D.runGithubRepo repo <> ".git"

checkLicense :: PackageMeta -> PrepareM ()
checkLicense pkgMeta =
  case bowerLicense pkgMeta of
    [] ->
      userError NoLicenseSpecified
    ls ->
      unless (any (isValidSPDX . T.unpack) ls)
        (userError InvalidLicense)

-- |
-- Check if a string is a valid SPDX license expression.
--
isValidSPDX :: String -> Bool
isValidSPDX input = case CabalParsec.simpleParsec input of
  Nothing -> False
  Just SPDX.NONE -> False
  Just _ -> True


extractGithub :: Text -> Maybe (D.GithubUser, D.GithubRepo)
extractGithub = stripGitHubPrefixes
   >>> fmap (T.splitOn "/")
   >=> takeTwo
   >>> fmap (D.GithubUser *** (D.GithubRepo . dropDotGit))

  where
  takeTwo :: [a] -> Maybe (a, a)
  takeTwo [x, y] = Just (x, y)
  takeTwo _ = Nothing

  stripGitHubPrefixes :: Text -> Maybe Text
  stripGitHubPrefixes = stripPrefixes [ "git://github.com/"
                                      , "https://github.com/"
                                      , "git@github.com:"
                                      ]

  stripPrefixes :: [Text] -> Text -> Maybe Text
  stripPrefixes prefixes str = msum $ (`T.stripPrefix` str) <$> prefixes

  dropDotGit :: Text -> Text
  dropDotGit str
    | ".git" `T.isSuffixOf` str = T.take (T.length str - 4) str
    | otherwise = str

readProcess' :: String -> [String] -> String -> PrepareM String
readProcess' prog args stdin = do
  out <- liftIO (catch (Right <$> readProcess prog args stdin)
                       (return . Left))
  either (otherError . ProcessFailed prog args) return out

data DependencyStatus
  = NoResolution
    -- ^ In the resolutions file, there was no _resolution key.
  | ResolvedOther Text
    -- ^ Resolved, but to something other than a version. The Text argument
    -- is the resolution type. The values it can take that I'm aware of are
    -- "commit" and "branch". Note: this constructor is deprecated, and is only
    -- used when parsing legacy resolutions files.
  | ResolvedVersion Version
    -- ^ Resolved to a version.
  deriving (Show, Eq)

parseResolutionsFile :: FilePath -> PrepareM [(PackageName, (FilePath, DependencyStatus))]
parseResolutionsFile resolutionsFile = do
  unlessM (liftIO (doesFileExist resolutionsFile)) (userError ResolutionsFileNotFound)
  depsBS <- liftIO (BL.readFile resolutionsFile)

  case parse asResolutions depsBS of
    Right res ->
      pure res
    Left err ->
      userError $ ResolutionsFileError resolutionsFile err

-- | Parser for resolutions files, which contain information about the packages
-- which this package depends on. A resolutions file should look something like
-- this:
--
-- {
--   "purescript-prelude": {
--      "version": "4.0.0",
--      "path": "bower_components/purescript-prelude"
--   },
--   "purescript-lists": {
--      "version": "6.0.0",
--      "path": "bower_components/purescript-lists"
--   },
--   ...
-- }
--
-- where the version is used for generating links between packages on Pursuit,
-- and the path is used to obtain the source files while generating
-- documentation: all files matching the glob "src/**/*.purs" relative to the
-- `path` directory will be picked up.
--
-- The "version" field is optional, but omitting it will mean that no links
-- will be generated for any declarations from that package on Pursuit. The
-- "path" field is required.
asResolutions :: Parse D.PackageError [(PackageName, (FilePath, DependencyStatus))]
asResolutions =
  eachInObjectWithKey parsePackageName $
    (,) <$> key "path" asString
        <*> (maybe NoResolution ResolvedVersion <$> keyMay "version" asVersion)

asVersion :: Parse D.PackageError Version
asVersion =
  withString (note D.InvalidVersion . D.parseVersion')

parsePackageName :: Text -> Either D.PackageError PackageName
parsePackageName = first D.ErrorInPackageMeta . Bower.parsePackageName

handleDeps
  :: [PackageName]
      -- ^ dependencies declared in package manifest file; we should emit
      -- warnings for any package name in this list which is not in the
      -- resolutions file.
  -> [(PackageName, DependencyStatus)]
      -- ^ Contents of resolutions file
  -> PrepareM [(PackageName, Version)]
handleDeps declared resolutions = do
  let missing = declared \\ map fst resolutions
  case missing of
    (x:xs) ->
      userError (MissingDependencies (x :| xs))
    [] -> do
      pkgs <-
        for resolutions $ \(pkgName, status) ->
          case status of
            NoResolution -> do
              warn (NoResolvedVersion pkgName)
              pure Nothing
            ResolvedOther other -> do
              warn (UnacceptableVersion (pkgName, other))
              pure Nothing
            ResolvedVersion version ->
              pure (Just (pkgName, version))
      pure (catMaybes pkgs)

getInputAndDepsFiles
  :: [(PackageName, FilePath)]
  -> IO ([FilePath], [(PackageName, FilePath)])
getInputAndDepsFiles depPaths = do
  inputFiles <- globRelative purescriptSourceFiles
  let handleDep (pkgName, path) = do
        depFiles <- globDir1 purescriptSourceFiles path
        return (map (pkgName,) depFiles)
  depFiles <- concat <$> traverse handleDep depPaths
  return (inputFiles, depFiles)
