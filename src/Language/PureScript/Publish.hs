{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , getModulesAndBookmarks
  , getResolvedDependencies
  ) where

import Prelude ()
import Prelude.Compat hiding (userError)

import Control.Arrow ((***))
import Control.Category ((>>>))
import Control.Exception (catch, try)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Writer.Strict

import Data.Aeson.BetterErrors
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (stripPrefix, isSuffixOf, (\\), nubBy)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Version
import qualified Data.SPDX as SPDX
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Safe (headMay)

import System.Directory (doesFileExist, findExecutable)
import System.Exit (exitFailure)
import System.FilePath (pathSeparator)
import System.Process (readProcess)
import qualified System.FilePath.Glob as Glob
import qualified System.Info

import Web.Bower.PackageMeta (PackageMeta(..), BowerError(..), PackageName, runPackageName, parsePackageName, Repository(..))
import qualified Web.Bower.PackageMeta as Bower

import Language.PureScript.Publish.ErrorsWarnings
import Language.PureScript.Publish.Utils
import qualified Language.PureScript as P (version)
import qualified Language.PureScript.Docs as D

data PublishOptions = PublishOptions
  { -- | How to obtain the version tag and version that the data being
    -- generated will refer to.
    publishGetVersion :: PrepareM (String, Version)
  , -- | What to do when the working tree is dirty
    publishWorkingTreeDirty :: PrepareM ()
  }

defaultPublishOptions :: PublishOptions
defaultPublishOptions = PublishOptions
  { publishGetVersion = getVersionFromGitTag
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

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond act = cond >>= flip unless act

preparePackage' :: PublishOptions -> PrepareM D.UploadedPackage
preparePackage' opts = do
  unlessM (liftIO (doesFileExist "bower.json")) (userError BowerJSONNotFound)
  checkCleanWorkingTree opts

  pkgMeta <- liftIO (Bower.decodeFile "bower.json")
                    >>= flip catchLeft (userError . CouldntDecodeBowerJSON)
  checkLicense pkgMeta

  (pkgVersionTag, pkgVersion) <- publishGetVersion opts
  pkgGithub                   <- getBowerRepositoryInfo pkgMeta
  (pkgBookmarks, pkgModules)  <- getModulesAndBookmarks

  let declaredDeps = map fst (bowerDependencies pkgMeta ++
                              bowerDevDependencies pkgMeta)
  pkgResolvedDependencies     <- getResolvedDependencies declaredDeps

  let pkgUploader = D.NotYetKnown
  let pkgCompilerVersion = P.version

  return D.Package{..}

getModulesAndBookmarks :: PrepareM ([D.Bookmark], [D.Module])
getModulesAndBookmarks = do
  (inputFiles, depsFiles) <- liftIO getInputAndDepsFiles
  (modules', bookmarks) <- parseAndBookmark inputFiles depsFiles

  case runExcept (D.convertModulesInPackage modules') of
    Right modules -> return (bookmarks, modules)
    Left err -> userError (CompileError err)

  where
  parseAndBookmark inputFiles depsFiles = do
    r <- liftIO . runExceptT $ D.parseAndBookmark inputFiles depsFiles
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

getVersionFromGitTag :: PrepareM (String, Version)
getVersionFromGitTag = do
  out <- readProcess' "git" ["tag", "--list", "--points-at", "HEAD"] ""
  let vs = map trimWhitespace (lines out)
  case mapMaybe parseMay vs of
    []  -> userError TagMustBeCheckedOut
    [x] -> return x
    xs  -> userError (AmbiguousVersions (map snd xs))
  where
  trimWhitespace =
    dropWhile isSpace >>> reverse >>> dropWhile isSpace >>> reverse
  parseMay str =
    (str,) <$> D.parseVersion' (dropPrefix "v" str)
  dropPrefix prefix str =
    fromMaybe str (stripPrefix prefix str)

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
      unless (any isValidSPDX ls)
        (userError InvalidLicense)

-- |
-- Check if a string is a valid SPDX license expression.
--
isValidSPDX :: String -> Bool
isValidSPDX = (== 1) . length . SPDX.parseExpression

extractGithub :: String -> Maybe (D.GithubUser, D.GithubRepo)
extractGithub = stripGitHubPrefixes
   >>> fmap (splitOn "/")
   >=> takeTwo
   >>> fmap (D.GithubUser *** (D.GithubRepo . dropDotGit))

  where
  takeTwo :: [a] -> Maybe (a, a)
  takeTwo [x, y] = Just (x, y)
  takeTwo _ = Nothing

  stripGitHubPrefixes :: String -> Maybe String
  stripGitHubPrefixes = stripPrefixes [ "git://github.com/"
                                      , "https://github.com/"
                                      , "git@github.com:"
                                      ]

  stripPrefixes :: [String] -> String -> Maybe String
  stripPrefixes prefixes str = msum $ (`stripPrefix` str) <$> prefixes

  dropDotGit :: String -> String
  dropDotGit str
    | ".git" `isSuffixOf` str = take (length str - 4) str
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
  | ResolvedOther String
    -- ^ Resolved, but to something other than a version. The String argument
    -- is the resolution type. The values it can take that I'm aware of are
    -- "commit" and "branch".
  | ResolvedVersion String
    -- ^ Resolved to a version. The String argument is the resolution tag (eg,
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
        (++) <$> eachInObjectWithKey (parsePackageName . T.unpack)
                                     asDependencyStatus
             <*> (concatMap snd <$> eachInObject asResolvedDependencies)

-- | Extracts only the top level dependency names from the output of
--   `bower list --json --offline`
asToplevelDependencies :: Parse BowerError [PackageName]
asToplevelDependencies =
  fmap (map fst) $
    key "dependencies" $
      eachInObjectWithKey (parsePackageName . T.unpack) (return ())

asDependencyStatus :: Parse e DependencyStatus
asDependencyStatus = do
  isMissing <- keyOrDefault "missing" False asBool
  if isMissing
    then
      return Missing
    else
      key "pkgMeta" $
        keyOrDefault "_resolution" NoResolution $ do
          type_ <- key "type" asString
          case type_ of
            "version" -> ResolvedVersion <$> key "tag" asString
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

  bowerDir pkgName = "bower_components/" ++ runPackageName pkgName

  -- Try to extract a version, and warn if unsuccessful.
  tryExtractVersion' pair =
    maybe (warn (UnacceptableVersion pair) >> return Nothing)
          (return . Just)
          (tryExtractVersion pair)

tryExtractVersion :: (PackageName, String) -> Maybe (PackageName, Version)
tryExtractVersion (pkgName, tag) =
  let tag' = fromMaybe tag (stripPrefix "v" tag)
  in  (pkgName,) <$> D.parseVersion' tag'

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
  case Bower.mkPackageName y of
    Right name -> Just name
    Left _ -> Nothing
