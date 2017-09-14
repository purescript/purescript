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
  , getResolvedDependencies
  ) where

import Protolude hiding (stdin)

import Control.Arrow ((***))
import Control.Category ((>>>))
import Control.Monad.Writer.Strict (MonadWriter, WriterT, runWriterT, tell)

import Data.Aeson.BetterErrors (Parse, parse, keyMay, eachInObjectWithKey, eachInObject, key, keyOrDefault, asBool, asString, asText)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.String (String, lines)
import Data.List (stripPrefix, (\\), nubBy)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Version
import qualified Data.SPDX as SPDX

import System.Directory (doesFileExist)
import System.FilePath.Glob (globDir1)
import System.Process (readProcess)

import Web.Bower.PackageMeta (PackageMeta(..), PackageName, parsePackageName, Repository(..))
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
  }

defaultPublishOptions :: PublishOptions
defaultPublishOptions = PublishOptions
  { publishGetVersion = getVersionFromGitTag
  , publishGetTagTime = getTagTime
  , publishWorkingTreeDirty = userError DirtyWorkingTree
  }

-- | Attempt to retrieve package metadata from the current directory.
-- Calls exitFailure if no package metadata could be retrieved.
unsafePreparePackage :: FilePath -> FilePath -> PublishOptions -> IO D.UploadedPackage
unsafePreparePackage manifestFile resolutionsFile opts =
  either (\e -> printError e >> exitFailure) pure
    =<< preparePackage manifestFile resolutionsFile opts

-- | Attempt to retrieve package metadata from the current directory.
-- Returns a PackageError on failure
preparePackage :: FilePath -> FilePath -> PublishOptions -> IO (Either PackageError D.UploadedPackage)
preparePackage manifestFile resolutionsFile opts =
  runPrepareM (preparePackage' manifestFile resolutionsFile opts)
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

preparePackage' :: FilePath -> FilePath -> PublishOptions -> PrepareM D.UploadedPackage
preparePackage' manifestFile resolutionsFile opts = do
  unlessM (liftIO (doesFileExist manifestFile)) (userError PackageManifestNotFound)
  checkCleanWorkingTree opts

  pkgMeta <- liftIO (Bower.decodeFile manifestFile)
                    >>= flip catchLeft (userError . CouldntDecodePackageManifest)
  checkLicense pkgMeta

  (pkgVersionTag, pkgVersion) <- publishGetVersion opts
  pkgTagTime                  <- Just <$> publishGetTagTime opts pkgVersionTag
  pkgGithub                   <- getManifestRepositoryInfo pkgMeta

  let declaredDeps = map fst (bowerDependencies pkgMeta ++
                              bowerDevDependencies pkgMeta)
  resolvedDeps                <- getResolvedDependencies resolutionsFile declaredDeps

  (pkgModules, pkgModuleMap)  <- getModules (map (second fst) resolvedDeps)

  let pkgUploader = D.NotYetKnown
  let pkgCompilerVersion = P.version
  let pkgResolvedDependencies = map (second snd) resolvedDeps

  return D.Package{..}

getModules
  :: [(PackageName, FilePath)]
  -> PrepareM ([D.Module], Map P.ModuleName PackageName)
getModules paths = do
  (inputFiles, depsFiles) <- liftIO (getInputAndDepsFiles paths)
  (modules', moduleMap) <- parseFilesInPackages inputFiles depsFiles

  case runExcept (D.convertModulesInPackage modules' moduleMap) of
    Right modules -> return (modules, moduleMap)
    Left err -> userError (CompileError err)

  where
  parseFilesInPackages inputFiles depsFiles = do
    r <- liftIO . runExceptT $ D.parseFilesInPackages inputFiles depsFiles
    case r of
      Right r' ->
        return r'
      Left err ->
        userError (CompileError err)

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
isValidSPDX = (== 1) . length . SPDX.parseExpression

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
  = Missing
    -- ^ Listed in package manifest, but not installed.
  | NoResolution
    -- ^ In the resolutions file, there was no _resolution key.
  | ResolvedOther Text
    -- ^ Resolved, but to something other than a version. The Text argument
    -- is the resolution type. The values it can take that I'm aware of are
    -- "commit" and "branch".
  | ResolvedVersion Text
    -- ^ Resolved to a version. The Text argument is the resolution tag (eg,
    -- "v0.1.0").
  deriving (Show, Eq)

-- Go through all dependencies which contain purescript code, and
-- extract their versions.
--
-- In the case where a dependency is taken from a particular version,
-- that's easy; take that version. In any other case (eg, a branch, or a commit
-- sha) we print a warning that documentation links will not work, and avoid
-- linking to documentation for any types from that package.
--
-- The rationale for this is: people will prefer to use a released version
-- where possible. If they are not using a released version, then this is
-- probably for a reason. However, docs are only ever available for released
-- versions. Therefore there will probably be no version of the docs which is
-- appropriate to link to, and we should omit links.
getResolvedDependencies :: FilePath -> [PackageName] -> PrepareM [(PackageName, (FilePath, Version))]
getResolvedDependencies resolutionsFile declaredDeps = do
  unlessM (liftIO (doesFileExist resolutionsFile)) (userError ResolutionsFileNotFound)
  depsBS <- liftIO (BL.readFile resolutionsFile)

  -- Check for undeclared dependencies
  toplevels <- catchJSON (parse asToplevelDependencies depsBS)
  warnUndeclared declaredDeps toplevels

  deps <- catchJSON (parse asResolvedDependencies depsBS)
  handleDeps deps

  where
  catchJSON = flip catchLeft (internalError . JSONError FromResolutions)

-- | Extracts all dependencies and their versions from a "resolutions" file, which
-- is based on the output of `bower list --json --offline`
asResolvedDependencies :: Parse D.ManifestError [(PackageName, (Maybe FilePath, DependencyStatus))]
asResolvedDependencies = nubBy ((==) `on` fst) <$> go
  where
  go =
    fmap (fromMaybe []) $
      keyMay "dependencies" $
        (++) <$> eachInObjectWithKey parsePackageName asDirectoryAndDependencyStatus
             <*> (concatMap snd <$> eachInObject asResolvedDependencies)

-- | Extracts only the top level dependency names from a resolutions file.
asToplevelDependencies :: Parse D.ManifestError [PackageName]
asToplevelDependencies =
  fmap (map fst) $
    key "dependencies" $
      eachInObjectWithKey parsePackageName (return ())

asDirectoryAndDependencyStatus :: Parse e (Maybe FilePath, DependencyStatus)
asDirectoryAndDependencyStatus = do
  isMissing <- keyOrDefault "missing" False asBool
  if isMissing
    then
      return (Nothing, Missing)
    else do
      directory <- key "canonicalDir" asString
      status <- key "pkgMeta" $
        keyOrDefault "_resolution" NoResolution $ do
          type_ <- key "type" asText
          case type_ of
            "version" -> ResolvedVersion <$> key "tag" asText
            other -> return (ResolvedOther other)
      return (Just directory, status)

warnUndeclared :: [PackageName] -> [PackageName] -> PrepareM ()
warnUndeclared declared actual =
  traverse_ (warn . UndeclaredDependency) (actual \\ declared)

handleDeps
  :: [(PackageName, (Maybe FilePath, DependencyStatus))]
  -> PrepareM [(PackageName, (FilePath, Version))]
handleDeps deps = do
  let (missing, noVersion, installed, missingPath) = partitionDeps deps
  case missing of
    (x:xs) ->
      userError (MissingDependencies (x :| xs))
    [] -> do
      traverse_ (warn . NoResolvedVersion) noVersion
      traverse_ (warn . MissingPath) missingPath
      catMaybes <$> traverse tryExtractVersion' installed

  where
  partitionDeps = foldr go ([], [], [], [])
  go (pkgName, (Nothing, _)) (ms, os, is, mp) =
    (ms, os, is, pkgName : mp)
  go (pkgName, (Just path, d)) (ms, os, is, mp) =
    case d of
      Missing           -> (pkgName : ms, os, is, mp)
      NoResolution      -> (ms, pkgName : os, is, mp)
      ResolvedOther _   -> (ms, pkgName : os, is, mp)
      ResolvedVersion v -> (ms, os, (pkgName, (path, v)) : is, mp)

  -- Try to extract a version, and warn if unsuccessful.
  tryExtractVersion'
    :: (PackageName, (extra, Text))
    -> PrepareM (Maybe (PackageName, (extra, Version)))
  tryExtractVersion' pair =
    maybe (warn (UnacceptableVersion (fmap snd pair)) >> return Nothing)
          (return . Just)
          (tryExtractVersion pair)

tryExtractVersion
  :: (PackageName, (extra, Text))
  -> Maybe (PackageName, (extra, Version))
tryExtractVersion (pkgName, (extra, tag)) =
  let tag' = fromMaybe tag (T.stripPrefix "v" tag)
  in  (pkgName,) . (extra,) <$> D.parseVersion' (T.unpack tag')

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
