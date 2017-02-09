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
  , getBowerRepositoryInfo
  , getModules
  , getResolvedDependencies
  ) where

import Protolude hiding (stdin)

import Control.Arrow ((***))
import Control.Category ((>>>))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Writer.Strict (MonadWriter, WriterT, runWriterT, tell)

import Data.Aeson.BetterErrors (Parse, parse, keyMay, eachInObjectWithKey, eachInObject, key, keyOrDefault, asBool, asText)
import Data.Char (isSpace)
import Data.String (String, lines)
import Data.List (stripPrefix, (\\), nubBy)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time.Clock (UTCTime)
import Data.Version
import qualified Data.SPDX as SPDX

import System.Directory (doesFileExist, findExecutable)
import System.FilePath (pathSeparator)
import System.Process (readProcess)
import qualified System.FilePath.Glob as Glob
import qualified System.Info

import Web.Bower.PackageMeta (PackageMeta(..), BowerError(..), PackageName, runPackageName, parsePackageName, Repository(..))
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
unsafePreparePackage :: PublishOptions -> IO D.UploadedPackage
unsafePreparePackage opts = either (\e -> printError e >> exitFailure) pure =<< preparePackage opts

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
  unlessM (liftIO (doesFileExist "bower.json")) (userError BowerJSONNotFound)
  checkCleanWorkingTree opts

  pkgMeta <- liftIO (Bower.decodeFile "bower.json")
                    >>= flip catchLeft (userError . CouldntDecodeBowerJSON)
  checkLicense pkgMeta

  (pkgVersionTag, pkgVersion) <- publishGetVersion opts
  pkgTagTime                  <- Just <$> publishGetTagTime opts pkgVersionTag
  pkgGithub                   <- getBowerRepositoryInfo pkgMeta
  (pkgModules, pkgModuleMap)  <- getModules

  let declaredDeps = map fst (bowerDependencies pkgMeta ++
                              bowerDevDependencies pkgMeta)
  pkgResolvedDependencies     <- getResolvedDependencies declaredDeps

  let pkgUploader = D.NotYetKnown
  let pkgCompilerVersion = P.version

  return D.Package{..}

getModules :: PrepareM ([D.Module], Map P.ModuleName PackageName)
getModules = do
  (inputFiles, depsFiles) <- liftIO getInputAndDepsFiles
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
  out <- readProcess' "git" ["show", T.unpack tag, "--no-patch", "--format=%aI"] ""
  case mapMaybe D.parseTime (lines out) of
    [t] -> pure t
    _ -> internalError (CouldntParseGitTagDate tag)

getBowerRepositoryInfo :: PackageMeta -> PrepareM (D.GithubUser, D.GithubRepo)
getBowerRepositoryInfo = either (userError . BadRepositoryField) return . tryExtract
  where
  tryExtract pkgMeta =
    case bowerRepository pkgMeta of
      Nothing -> Left RepositoryFieldMissing
      Just Repository{..} -> do
        unless (repositoryType == "git")
          (Left (BadRepositoryType repositoryType))
        maybe (Left NotOnGithub) Right (extractGithub repositoryUrl)

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
    -- ^ Listed in bower.json, but not installed.
  | NoResolution
    -- ^ In the output of `bower list --json --offline`, there was no
    -- _resolution key. This can be caused by adding the dependency using
    -- `bower link`, or simply copying it into bower_components instead of
    -- installing it normally.
  | ResolvedOther Text
    -- ^ Resolved, but to something other than a version. The Text argument
    -- is the resolution type. The values it can take that I'm aware of are
    -- "commit" and "branch".
  | ResolvedVersion Text
    -- ^ Resolved to a version. The Text argument is the resolution tag (eg,
    -- "v0.1.0").
  deriving (Show, Eq)

-- Go through all bower dependencies which contain purescript code, and
-- extract their versions.
--
-- In the case where a bower dependency is taken from a particular version,
-- that's easy; take that version. In any other case (eg, a branch, or a commit
-- sha) we print a warning that documentation links will not work, and avoid
-- linking to documentation for any types from that package.
--
-- The rationale for this is: people will prefer to use a released version
-- where possible. If they are not using a released version, then this is
-- probably for a reason. However, docs are only ever available for released
-- versions. Therefore there will probably be no version of the docs which is
-- appropriate to link to, and we should omit links.
getResolvedDependencies :: [PackageName] -> PrepareM [(PackageName, Version)]
getResolvedDependencies declaredDeps = do
  bower <- findBowerExecutable
  depsBS <- packUtf8 <$> readProcess' bower ["list", "--json", "--offline"] ""

  -- Check for undeclared dependencies
  toplevels <- catchJSON (parse asToplevelDependencies depsBS)
  warnUndeclared declaredDeps toplevels

  deps <- catchJSON (parse asResolvedDependencies depsBS)
  handleDeps deps

  where
  packUtf8 = TL.encodeUtf8 . TL.pack
  catchJSON = flip catchLeft (internalError . JSONError FromBowerList)

findBowerExecutable :: PrepareM String
findBowerExecutable = do
  mname <- liftIO . runMaybeT . msum . map (MaybeT . findExecutable) $ names
  maybe (userError (BowerExecutableNotFound names)) return mname
  where
  names = case System.Info.os of
    "mingw32" -> ["bower", "bower.cmd"]
    _         -> ["bower"]

-- | Extracts all dependencies and their versions from
--   `bower list --json --offline`
asResolvedDependencies :: Parse BowerError [(PackageName, DependencyStatus)]
asResolvedDependencies = nubBy ((==) `on` fst) <$> go
  where
  go =
    fmap (fromMaybe []) $
      keyMay "dependencies" $
        (++) <$> eachInObjectWithKey parsePackageName asDependencyStatus
             <*> (concatMap snd <$> eachInObject asResolvedDependencies)

-- | Extracts only the top level dependency names from the output of
--   `bower list --json --offline`
asToplevelDependencies :: Parse BowerError [PackageName]
asToplevelDependencies =
  fmap (map fst) $
    key "dependencies" $
      eachInObjectWithKey parsePackageName (return ())

asDependencyStatus :: Parse e DependencyStatus
asDependencyStatus = do
  isMissing <- keyOrDefault "missing" False asBool
  if isMissing
    then
      return Missing
    else
      key "pkgMeta" $
        keyOrDefault "_resolution" NoResolution $ do
          type_ <- key "type" asText
          case type_ of
            "version" -> ResolvedVersion <$> key "tag" asText
            other -> return (ResolvedOther other)

warnUndeclared :: [PackageName] -> [PackageName] -> PrepareM ()
warnUndeclared declared actual =
  traverse_ (warn . UndeclaredDependency) (actual \\ declared)

handleDeps ::
  [(PackageName, DependencyStatus)] -> PrepareM [(PackageName, Version)]
handleDeps deps = do
  let (missing, noVersion, installed) = partitionDeps deps
  case missing of
    (x:xs) ->
      userError (MissingDependencies (x :| xs))
    [] -> do
      traverse_ (warn . NoResolvedVersion) noVersion
      withVersions <- catMaybes <$> traverse tryExtractVersion' installed
      filterM (liftIO . isPureScript . bowerDir . fst) withVersions

  where
  partitionDeps = foldr go ([], [], [])
  go (pkgName, d) (ms, os, is) =
    case d of
      Missing           -> (pkgName : ms, os, is)
      NoResolution      -> (ms, pkgName : os, is)
      ResolvedOther _   -> (ms, pkgName : os, is)
      ResolvedVersion v -> (ms, os, (pkgName, v) : is)

  bowerDir pkgName = T.unpack $ "bower_components/" <> runPackageName pkgName

  -- Try to extract a version, and warn if unsuccessful.
  tryExtractVersion' :: (PackageName, Text) -> PrepareM (Maybe (PackageName, Version))
  tryExtractVersion' pair =
    maybe (warn (UnacceptableVersion pair) >> return Nothing)
          (return . Just)
          (tryExtractVersion pair)

tryExtractVersion :: (PackageName, Text) -> Maybe (PackageName, Version)
tryExtractVersion (pkgName, tag) =
  let tag' = fromMaybe tag (T.stripPrefix "v" tag)
  in  (pkgName,) <$> D.parseVersion' (T.unpack tag')

-- | Returns whether it looks like there is a purescript package checked out
-- in the given directory.
isPureScript :: FilePath -> IO Bool
isPureScript dir = do
  files <- Glob.globDir1 purescriptSourceFiles dir
  return (not (null files))

getInputAndDepsFiles :: IO ([FilePath], [(PackageName, FilePath)])
getInputAndDepsFiles = do
  inputFiles <- globRelative purescriptSourceFiles
  depsFiles' <- globRelative purescriptDepsFiles
  return (inputFiles, mapMaybe withPackageName depsFiles')

withPackageName :: FilePath -> Maybe (PackageName, FilePath)
withPackageName fp = (,fp) <$> getPackageName fp

getPackageName :: FilePath -> Maybe PackageName
getPackageName fp = do
  let xs = splitOn [pathSeparator] fp
  ys <- stripPrefix ["bower_components"] xs
  y <- headMay ys
  case Bower.mkPackageName (T.pack y) of
    Right name -> Just name
    Left _ -> Nothing
