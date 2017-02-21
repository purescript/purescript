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
  , getModules
  ) where

import Protolude hiding (stdin)

import Control.Arrow ((***))
import Control.Category ((>>>))
import Control.Monad.Writer.Strict (MonadWriter, WriterT, runWriterT, tell)

import Data.Aeson.BetterErrors (parse)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.String (String, lines)
import Data.List (stripPrefix)
import Data.List.Split (splitOn)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Version
import qualified Data.SPDX as SPDX

import System.FilePath (pathSeparator)
import System.Process (readProcess)

import Language.PureScript.Publish.ErrorsWarnings
import Language.PureScript.Publish.Utils
import qualified Language.PureScript as P (version, ModuleName)
import qualified Language.PureScript.Docs as D

data PublishOptions = PublishOptions
  { publishGetVersion :: PrepareM (Text, Version)
    -- ^ How to obtain the version tag and version that the data being
    -- generated will refer to.
  , publishGetTagTime :: Text -> PrepareM UTCTime
  , publishWorkingTreeDirty :: PrepareM ()
    -- ^ What to do when the working tree is dirty
  }

defaultPublishOptions :: PublishOptions
defaultPublishOptions = PublishOptions
  { publishGetVersion = getVersionFromGitTag
  , publishGetTagTime = getTagTime
  , publishWorkingTreeDirty = userError DirtyWorkingTree
  }

-- | Attempt to retrieve package metadata from the current directory.
-- Calls exitFailure if no package metadata could be retrieved.
unsafePreparePackage :: FilePath -> PublishOptions -> IO D.UploadedPackage
unsafePreparePackage manifestPath opts =
  either (\e -> printError e >> exitFailure) pure =<< preparePackage manifestPath opts

-- | Attempt to retrieve package metadata from the current directory.
-- Returns a PackageError on failure
preparePackage :: FilePath -> PublishOptions -> IO (Either PackageError D.UploadedPackage)
preparePackage manifestPath opts =
  runPrepareM (preparePackage' manifestPath opts)
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

preparePackage' :: FilePath -> PublishOptions -> PrepareM D.UploadedPackage
preparePackage' manifestPath opts = do
  checkCleanWorkingTree opts

  pkgMeta <- liftIO (fmap (parse D.asPackageMeta) (BL.readFile manifestPath))
                    >>= flip catchLeft (userError . CouldntDecodeManifest)
  checkLicense pkgMeta

  (pkgVersionTag, pkgVersion) <- publishGetVersion opts
  pkgTagTime                  <- Just <$> publishGetTagTime opts pkgVersionTag
  pkgGithub                   <- getRepositoryInfo pkgMeta
  (pkgModules, pkgModuleMap)  <- getModules

  let pkgUploader = D.NotYetKnown
  let pkgCompilerVersion = P.version

  return D.Package{..}

getModules :: PrepareM ([D.Module], Map P.ModuleName D.PackageName)
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
  out <- readProcess' "git" ["tag", "-l", T.unpack tag, "--format=%(taggerdate:unix)"] ""
  case mapMaybe readMaybe (lines out) of
    [t] -> pure . posixSecondsToUTCTime . fromInteger $ t
    _ -> internalError (CouldntParseGitTagDate tag)

getRepositoryInfo :: D.PackageMeta -> PrepareM (D.GithubUser, D.GithubRepo)
getRepositoryInfo = either (userError . BadRepositoryField) return . tryExtract
  where
  tryExtract pkgMeta =
    maybe (Left NotOnGithub) Right (extractGithub (D.packageMetaRepository pkgMeta))

checkLicense :: D.PackageMeta -> PrepareM ()
checkLicense pkgMeta =
  case D.packageMetaLicense pkgMeta of
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

getInputAndDepsFiles :: IO ([FilePath], [(D.PackageName, FilePath)])
getInputAndDepsFiles = do
  inputFiles <- globRelative purescriptSourceFiles
  depsFiles' <- globRelative purescriptDepsFiles
  return (inputFiles, mapMaybe withPackageName depsFiles')

withPackageName :: FilePath -> Maybe (D.PackageName, FilePath)
withPackageName fp = (,fp) <$> getPackageName fp

getPackageName :: FilePath -> Maybe D.PackageName
getPackageName fp = do
  let xs = splitOn [pathSeparator] fp
  ys <- stripPrefix ["bower_components"] xs
  y <- headMay ys
  pure (D.PackageName (T.pack y))
