{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Prelude hiding (userError)

import Data.Maybe
import Data.Char (isSpace)
import Data.String (fromString)
import Data.List (stripPrefix, isSuffixOf, intercalate)
import Data.List.Split (splitOn)
import Data.Version

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

import qualified Data.Aeson as A
import Data.Aeson.BetterErrors

import Control.Applicative
import Control.Category ((>>>))
import Control.Arrow ((***))
import Control.Exception (catch, IOException)
import Control.Monad.Trans.Except
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer
import Control.Lens ((^?))

import System.Directory (doesFileExist)
import System.Process (readProcess)
import System.Exit (exitFailure)
import qualified System.FilePath.Glob as Glob
import Network.Wreq (get, responseBody)
import Network.HTTP.Client (HttpException(StatusCodeException))
import Network.HTTP.Types (notFound404)
import Web.Bower.PackageMeta hiding (Version)

import qualified Language.PureScript.Docs as D
import BoxesHelpers
import Output (parseAndDesugar, getInputAndDepsFiles)

main :: IO ()
main = do
  pkg <- preparePackage
  BL.putStrLn (A.encode pkg)

-- | Attempt to retrieve package metadata from the current directory.
-- Calls exitFailure if no package metadata could be retrieved.
preparePackage :: IO D.UploadedPackage
preparePackage =
  runPrepareM preparePackage'
    >>= either (\e -> printError e >> exitFailure)
               handleWarnings
  where
  printError err = do
    printBox $ case err of
      UserError e ->
        vcat
          [ para (concat
            [ "There is a problem with your package, which meant that "
            , "documentation could not be generated."
            ])
          , para "Details:"
          , indented (displayUserError e)
          ]
      InternalError e -> do
        vcat
          [ para "Internal error: this is probably a bug. Please report it:"
          , indented (para "https://github.com/purescript/pursuit/issues/new")
          , spacer
          , para "Details:"
          , successivelyIndented (displayInternalError e)
          ]
      OtherError e -> do
        vcat
          [ para "An error occurred."
          , para "Details:"
          , indented (displayOtherError e)
          ]

  handleWarnings (x, warns) = do
    let names = map getPackageName warns
    case length warns of
      0 -> return ()
      1 -> pluraliseMsg "package" "was" "this" "this" names
      _ -> pluraliseMsg "packages" "were" "any of these" "these" names
    return x

  getPackageName :: PackageWarning -> String
  getPackageName (ResolutionNotVersion n) = runPackageName n

  pluraliseMsg packages were anyOfThese these names =
    printBox $ vcat $
      [ para (concat
        ["The following ", packages, " ", were, " not resolved to a version:"])
      ] ++ (map (indented . para) names) ++
      [ spacer
      , para (concat
        ["Links to types in ", anyOfThese, " ", packages, " will not work. In "
        , "order to make links work, edit your bower.json to specify a version"
        , " or a version range for ", these, " ", packages, ", and rerun "
        , "`bower install`."
        ])
      ]

-- | An error which meant that it was not possible to retrieve metadata for a
-- package.
data PackageError
  = UserError UserError
  | InternalError InternalError
  | OtherError OtherError
  deriving (Show)

-- | An error that should be fixed by the user.
data UserError
  = BowerJSONNotFound
  | CouldntParseBowerJSON (ParseError BowerError)
  | BowerJSONNameMissing
  | TagMustBeCheckedOut
  | AmbiguousVersions [Version]
  | NotOnGithub
  | NotOnBower PackageName
  deriving (Show)

displayUserError :: UserError -> Box
displayUserError e = case e of
  BowerJSONNotFound ->
    para (concat
      [ "The bower.json file was not found. Please create one, or run "
      , "`pulp init`."
      ])
  CouldntParseBowerJSON err ->
    vcat
      [ successivelyIndented
        ["The bower.json file could not be parsed as JSON:", "aeson reported: " ++ show err]
      , para "Please ensure that your bower.json file is valid JSON."
      ]
  BowerJSONNameMissing ->
    vcat
      [ successivelyIndented ["In bower.json:", "the \"name\" key was not found."]
      , para "Please give your package a name first."
      ]
  TagMustBeCheckedOut ->
      vcat
        [ para (concat
            [ "psc-pages requires a tagged version to be checked out in order "
            , "to build documentation, and no suitable tag was found. Please "
            , "check out a previously tagged version, or tag a new version."
            ])
        , spacer
        , para "Note: tagged versions must be in one of the following forms:"
        , indented (para "* v{MAJOR}.{MINOR}.{PATCH} (example: \"v1.6.2\")")
        , indented (para "* {MAJOR}.{MINOR}.{PATCH} (example: \"1.6.2\")")
        ]
  AmbiguousVersions vs ->
    vcat $
      [ para (concat
          [ "The currently checked out commit seems to have been tagged with "
          , "more than 1 version, and I don't know which one should be used. "
          , "Please either delete some of the tags, or use a different commit "
          , "to tag the desired verson with."
          ])
      , spacer
      , para "Tags for the currently checked out commit:"
      ] ++ map (indented . para . ("* " ++) . showVersion) vs
  NotOnGithub ->
    para (concat
      [ "The entry for this package in the Bower registry does not point to a "
      , "GitHub repository. Currently, pursuit does not support packages which "
      , "are not hosted on GitHub. If you would prefer not to host your "
      , "package on GitHub, please open an issue: "
      , "https://github.com/purescript/pursuit/issues/new"
      ])
  NotOnBower name ->
    para (concat
      [ "Your package (" ++ runPackageName name ++ ") does not yet appear to "
      , "be on the Bower registry. Please add it before continuing."
      ])

-- | An error that probably indicates a bug in this module.
data InternalError
  = JSONError JSONSource (ParseError BowerError)
  deriving (Show)

displayInternalError :: InternalError -> [String]
displayInternalError e = case e of
  JSONError src r ->
    ["Error in JSON " ++ displayJSONSource src ++ ":"] ++
      map T.unpack (disp (const "No error text available.") r)
  where
  disp = Data.Aeson.BetterErrors.displayError

data JSONSource
  = FromFile FilePath
  | FromBowerListPaths
  | FromBowerApi
  deriving (Show)

displayJSONSource :: JSONSource -> String
displayJSONSource s = case s of
  FromFile fp ->
    "in file " ++ show fp
  FromBowerListPaths ->
    "in the output of `bower list --paths`"
  FromBowerApi ->
    "from the Bower registry API"

data OtherError
  = HttpExceptionThrown HttpException
  | ProcessFailed String [String] IOException
  deriving (Show)

displayOtherError :: OtherError -> Box
displayOtherError e = case e of
  HttpExceptionThrown exc ->
    successivelyIndented
      [ "HTTP exception:", show exc ]
  ProcessFailed prog args exc ->
    successivelyIndented
      [ "While running `" ++ prog ++ " " ++ intercalate " " args ++ "`:"
      , show exc
      ]

data PackageWarning
  = ResolutionNotVersion PackageName
  deriving (Show)


type PrepareM = WriterT [PackageWarning] (ExceptT PackageError IO)

runPrepareM :: PrepareM a -> IO (Either PackageError (a, [PackageWarning]))
runPrepareM = runExceptT . runWriterT

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

preparePackage' :: PrepareM D.UploadedPackage
preparePackage' = do
  exists <- liftIO (doesFileExist "bower.json")
  unless exists (userError BowerJSONNotFound)

  pkgMeta <- liftIO (decodeFile "bower.json")
                    >>= flip catchLeft (userError . CouldntParseBowerJSON)
  pkgVersion              <- getVersionFromGitTag
  pkgGithub               <- getBowerInfo (bowerName pkgMeta)
  pkgResolvedDependencies <- getBowerDepsVersions
  pkgModuleSet            <- liftIO getRenderedModuleSet
  let pkgUploader = Nothing

  return D.UploadedPackage{..}

getRenderedModuleSet :: IO D.RenderedModuleSet
getRenderedModuleSet = do
  (inputFiles, depsFiles) <- getInputAndDepsFiles
  (ms, depms, bs) <- parseAndDesugar inputFiles depsFiles $ \depms bookmarks ms ->
                        return (ms, depms, bookmarks)
  return (map D.renderModule ms, depms, bs)

getVersionFromGitTag :: PrepareM Version
getVersionFromGitTag = do
  out <- readProcess' "git" ["tag", "--list", "--points-at", "HEAD"] ""
  let vs = map clean (lines out)
  case mapMaybe D.parseVersion' vs of
    []  -> userError TagMustBeCheckedOut
    [x] -> return x
    xs  -> userError (AmbiguousVersions xs)
  where
  clean =
    trimWhitespace >>> dropPrefix "v"
  trimWhitespace =
    dropWhile isSpace >>> reverse >>> dropWhile isSpace >>> reverse
  dropPrefix prefix str =
    fromMaybe str (stripPrefix prefix str)

getBowerInfo :: PackageName -> PrepareM (D.GithubUser, D.GithubRepo)
getBowerInfo pkgName = do
  r' <- liftIO (catch (Right <$> get (bowerPackageUrl pkgName))
                      (return . Left))
  r <- either handleError return r'

  case getUrl r of
    Right url ->
      maybe (userError NotOnGithub) return (extractGithub url)
    Left err ->
      internalError (JSONError FromBowerApi err)

  where
  handleError (StatusCodeException status _ _)
    | status == notFound404 = userError (NotOnBower pkgName)
  handleError e = otherError (HttpExceptionThrown e)

  getUrl r =
    parse (key "url" asString)
          (fromMaybe "" (r ^? responseBody))

bowerDomainName :: String
bowerDomainName = "bower.herokuapp.com"

bowerPackageUrl :: PackageName -> String
bowerPackageUrl name =
  concat [ "https://"
         , bowerDomainName
         , "/packages/"
         , runPackageName name
         ]

extractGithub :: String -> Maybe (D.GithubUser, D.GithubRepo)
extractGithub =
  stripPrefix "git://github.com/"
   >>> fmap (splitOn "/")
   >=> takeTwo
   >>> fmap (D.GithubUser *** (D.GithubRepo . dropDotGit))

  where
  takeTwo :: [a] -> Maybe (a, a)
  takeTwo [x, y] = Just (x, y)
  takeTwo _ = Nothing

  dropDotGit :: String -> String
  dropDotGit str
    | ".git" `isSuffixOf` str = take (length str - 4) str
    | otherwise = str

readProcess' :: String -> [String] -> String -> PrepareM String
readProcess' prog args stdin = do
  out <- liftIO (catch (Right <$> readProcess prog args stdin)
                       (return . Left))
  either (otherError . ProcessFailed prog args) return out

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
getBowerDepsVersions :: PrepareM [(PackageName, Version)]
getBowerDepsVersions = do
  pathsBS <- fromString <$> readProcess' "bower" ["list", "--paths", "--json"] ""

  paths <- catchLeft (parse asBowerPathList pathsBS)
                     (internalError . JSONError FromBowerListPaths)

  catMaybes <$> mapM getVersion paths

  where
  asBowerPathList :: Parse BowerError [(PackageName, String)]
  asBowerPathList = eachInObjectWithKey (parsePackageName . T.unpack) asString

  getVersion :: (PackageName, String) -> PrepareM (Maybe (PackageName, Version))
  getVersion (pkgName, path) = do
    let jsonPath = path ++ "/.bower.json"

    let malformedJSON = internalError . JSONError (FromFile jsonPath)

    isPs <- liftIO (isPureScript path)
    if not isPs
      then return Nothing
      else do
        jsonBS <- liftIO (B.readFile jsonPath)

        (typ, tag) <- catchLeft (getTypeAndTag jsonBS)
                                malformedJSON
        case typ of
          "version" -> do
            let tag' = fromMaybe tag (stripPrefix "v" tag)
            return ((pkgName,) <$> D.parseVersion' tag')
          _ -> do
            warn (ResolutionNotVersion pkgName)
            return Nothing

  -- | Returns whether it looks like there is a purescript package checked out
  -- in the given directory.
  isPureScript :: FilePath -> IO Bool
  isPureScript dir = do
    files <- liftIO (Glob.globDir1 psSourceFiles dir)
    return (not (null files))

  -- | Get the resolution type and tag from the JSON
  getTypeAndTag =
    parseStrict (key "_resolution"
      ((,) <$> key "type" asString
           <*> key "tag" asString))

psSourceFiles :: Glob.Pattern
psSourceFiles = Glob.compile "src/**/*.purs"

psDepsFiles :: Glob.Pattern
psDepsFiles = Glob.compile "bower-components/*/src/**/*.purs"
