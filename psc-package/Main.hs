{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Control.Foldl as Foldl
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Foldable (fold, for_, traverse_)
import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import           Data.Text (pack)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Read as TR
import           Data.Traversable (for)
import           Data.Version (showVersion)
import qualified Filesystem.Path.CurrentOS as Path
import           GHC.Generics (Generic)
import qualified Options.Applicative as Opts
import qualified Paths_purescript as Paths
import qualified System.IO as IO
import           Turtle hiding (fold, s, x)
import qualified Turtle

packageFile :: Path.FilePath
packageFile = "psc-package.json"

data PackageConfig = PackageConfig
  { name    :: Text
  , depends :: [Text]
  , set     :: Text
  , source  :: Text
  } deriving (Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

pathToTextUnsafe :: Turtle.FilePath -> Text
pathToTextUnsafe = either (error "Path.toText failed") id . Path.toText

defaultPackage :: Text -> PackageConfig
defaultPackage pkgName =
  PackageConfig { name    = pkgName
                , depends = [ "prelude" ]
                , set     = "psc-" <> pack (showVersion Paths.version)
                , source  = "https://github.com/purescript/package-sets.git"
                }

readPackageFile :: IO PackageConfig
readPackageFile = do
  exists <- testfile packageFile
  unless exists $ do
    echo "psc-package.json does not exist"
    exit (ExitFailure 1)
  mpkg <- Aeson.decodeStrict . encodeUtf8 <$> readTextFile packageFile
  case mpkg of
    Nothing -> do
      echo "Unable to parse psc-package.json"
      exit (ExitFailure 1)
    Just pkg -> return pkg

packageConfigToJSON :: PackageConfig -> Text
packageConfigToJSON =
    TL.toStrict
    . TB.toLazyText
    . encodePrettyToTextBuilder' config
  where
    config = defConfig
               { confCompare =
                   keyOrder [ "name"
                            , "set"
                            , "source"
                            , "depends"
                            ]
               }

packageSetToJSON :: PackageSet -> Text
packageSetToJSON =
    TL.toStrict
    . TB.toLazyText
    . encodePrettyToTextBuilder' config
  where
    config = defConfig { confCompare = compare }

writePackageFile :: PackageConfig -> IO ()
writePackageFile =
  writeTextFile packageFile
  . packageConfigToJSON

data PackageInfo = PackageInfo
  { repo         :: Text
  , version      :: Text
  , dependencies :: [Text]
  } deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

type PackageSet = Map.Map Text PackageInfo

cloneShallow
  :: Text
  -- ^ repo
  -> Text
  -- ^ branch/tag
  -> Turtle.FilePath
  -- ^ target directory
  -> IO ExitCode
cloneShallow from ref into =
  proc "git"
       [ "clone"
       , "-q"
       , "-c", "advice.detachedHead=false"
       , "--depth", "1"
       , "-b", ref
       , from
       , pathToTextUnsafe into
       ] empty .||. exit (ExitFailure 1)

listRemoteTags
  :: Text
  -- ^ repo
  -> Turtle.Shell Text
listRemoteTags from =
  inproc "git"
         [ "ls-remote"
         , "-q"
         , "-t"
         , from
         ] empty

getPackageSet :: PackageConfig -> IO ()
getPackageSet PackageConfig{ source, set } = do
  let pkgDir = ".psc-package" </> fromText set </> ".set"
  exists <- testdir pkgDir
  unless exists . void $ cloneShallow source set pkgDir

readPackageSet :: PackageConfig -> IO PackageSet
readPackageSet PackageConfig{ set } = do
  let dbFile = ".psc-package" </> fromText set </> ".set" </> "packages.json"
  exists <- testfile dbFile
  unless exists $ do
    echo $ format (fp%" does not exist") dbFile
    exit (ExitFailure 1)
  mdb <- Aeson.decodeStrict . encodeUtf8 <$> readTextFile dbFile
  case mdb of
    Nothing -> do
      echo "Unable to parse packages.json"
      exit (ExitFailure 1)
    Just db -> return db

writePackageSet :: PackageConfig -> PackageSet -> IO ()
writePackageSet PackageConfig{ set } =
  let dbFile = ".psc-package" </> fromText set </> ".set" </> "packages.json"
  in writeTextFile dbFile . packageSetToJSON

installOrUpdate :: Text -> Text -> PackageInfo -> IO Turtle.FilePath
installOrUpdate set pkgName PackageInfo{ repo, version } = do
  echo ("Updating " <> pkgName)
  let pkgDir = ".psc-package" </> fromText set </> fromText pkgName </> fromText version
  exists <- testdir pkgDir
  unless exists . void $ cloneShallow repo version pkgDir
  pure pkgDir

getTransitiveDeps :: PackageSet -> [Text] -> IO [(Text, PackageInfo)]
getTransitiveDeps db depends = do
  pkgs <- for depends $ \pkg ->
    case Map.lookup pkg db of
      Nothing -> do
        echo ("Package " <> pkg <> " does not exist in package set")
        exit (ExitFailure 1)
      Just PackageInfo{ dependencies } -> return (pkg : dependencies)
  let unique = Set.toList (foldMap Set.fromList pkgs)
  return (mapMaybe (\name -> fmap (name, ) (Map.lookup name db)) unique)

updateImpl :: PackageConfig -> IO ()
updateImpl config@PackageConfig{ depends } = do
  getPackageSet config
  db <- readPackageSet config
  trans <- getTransitiveDeps db depends
  echo ("Updating " <> pack (show (length trans)) <> " packages...")
  for_ trans $ \(pkgName, pkg) -> installOrUpdate (set config) pkgName pkg

initialize :: IO ()
initialize = do
  exists <- testfile "psc-package.json"
  when exists $ do
    echo "psc-package.json already exists"
    exit (ExitFailure 1)
  echo "Initializing new project in current directory"
  pkgName <- pathToTextUnsafe . Path.filename <$> pwd
  let pkg = defaultPackage pkgName
  writePackageFile pkg
  updateImpl pkg

update :: IO ()
update = do
  pkg <- readPackageFile
  updateImpl pkg
  echo "Update complete"

install :: String -> IO ()
install pkgName = do
  pkg <- readPackageFile
  let pkg' = pkg { depends = nub (pack pkgName : depends pkg) }
  updateImpl pkg'
  writePackageFile pkg'
  echo "psc-package.json file was updated"

uninstall :: String -> IO ()
uninstall pkgName = do
  pkg <- readPackageFile
  let pkg' = pkg { depends = filter (/= pack pkgName) $ depends pkg }
  updateImpl pkg'
  writePackageFile pkg'
  echo "psc-package.json file was updated"

listDependencies :: IO ()
listDependencies = do
  pkg@PackageConfig{ depends } <- readPackageFile
  db <- readPackageSet pkg
  trans <- getTransitiveDeps db depends
  traverse_ (echo . fst) trans

listPackages :: IO ()
listPackages = do
  pkg <- readPackageFile
  db <- readPackageSet pkg
  traverse_ echo (fmt <$> Map.assocs db)
  where
  fmt :: (Text, PackageInfo) -> Text
  fmt (name, PackageInfo{ version }) = name <> " (" <> version <> ")"

getSourcePaths :: PackageConfig -> PackageSet -> [Text] -> IO [Turtle.FilePath]
getSourcePaths PackageConfig{..} db pkgNames = do
  trans <- getTransitiveDeps db pkgNames
  let paths = [ ".psc-package"
                </> fromText set
                </> fromText pkgName
                </> fromText version
                </> "src" </> "**" </> "*.purs"
              | (pkgName, PackageInfo{ version }) <- trans
              ]
  return paths

listSourcePaths :: IO ()
listSourcePaths = do
  pkg@PackageConfig{ depends } <- readPackageFile
  db <- readPackageSet pkg
  paths <- getSourcePaths pkg db depends
  traverse_ (echo . pathToTextUnsafe) paths

exec :: Text -> IO ()
exec exeName = do
  pkg@PackageConfig{..} <- readPackageFile
  db <- readPackageSet pkg
  paths <- getSourcePaths pkg db depends
  procs exeName
        (map pathToTextUnsafe ("src" </> "**" </> "*.purs" : paths))
        empty

checkForUpdates :: Bool -> Bool -> IO ()
checkForUpdates applyMinorUpdates applyMajorUpdates = do
    pkg <- readPackageFile
    db <- readPackageSet pkg

    echo ("Checking " <> pack (show (Map.size db)) <> " packages for updates.")
    echo "Warning: this could take some time!"

    newDb <- Map.fromList <$> (for (Map.toList db) $ \(name, p@PackageInfo{ repo, version }) -> do
      echo ("Checking package " <> name)
      tagLines <- Turtle.fold (listRemoteTags repo) Foldl.list
      let tags = mapMaybe parseTag tagLines
      newVersion <- case parseVersion version of
        Just parts ->
          let applyMinor =
                case filter (isMinorReleaseFrom parts) tags of
                  [] -> pure version
                  minorReleases -> do
                    echo ("New minor release available")
                    case applyMinorUpdates of
                      True -> do
                        let latestMinorRelease = maximum minorReleases
                        pure ("v" <> T.intercalate "." (map (pack . show) latestMinorRelease))
                      False -> pure version
              applyMajor =
                case filter (isMajorReleaseFrom parts) tags of
                  [] -> applyMinor
                  newReleases -> do
                    echo ("New major release available")
                    case applyMajorUpdates of
                      True -> do
                        let latestRelease = maximum newReleases
                        pure ("v" <> T.intercalate "." (map (pack . show) latestRelease))
                      False -> applyMinor
          in applyMajor
        _ -> do
          echo "Unable to parse version string"
          pure version
      pure (name, p { version = newVersion }))

    when (applyMinorUpdates || applyMajorUpdates)
      (writePackageSet pkg newDb)
  where
    parseTag :: Text -> Maybe [Int]
    parseTag line =
      case T.splitOn "\t" line of
        [_sha, ref] ->
          case T.stripPrefix "refs/tags/" ref of
            Just tag ->
              case parseVersion tag of
                Just parts -> pure parts
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing

    parseVersion :: Text -> Maybe [Int]
    parseVersion ref =
      case T.stripPrefix "v" ref of
        Just tag ->
          traverse parseDecimal (T.splitOn "." tag)
        _ -> Nothing

    parseDecimal :: Text -> Maybe Int
    parseDecimal s =
      case TR.decimal s of
        Right (n, "") -> Just n
        _ -> Nothing

    isMajorReleaseFrom :: [Int] -> [Int] -> Bool
    isMajorReleaseFrom (0 : xs) (0 : ys) = isMajorReleaseFrom xs ys
    isMajorReleaseFrom (x : _)  (y : _)  = y > x
    isMajorReleaseFrom _        _        = False

    isMinorReleaseFrom :: [Int] -> [Int] -> Bool
    isMinorReleaseFrom (0 : xs) (0 : ys) = isMinorReleaseFrom xs ys
    isMinorReleaseFrom (x : xs) (y : ys) = y == x && ys > xs
    isMinorReleaseFrom _        _        = False

verifyPackageSet :: IO ()
verifyPackageSet = do
  pkg <- readPackageFile
  db <- readPackageSet pkg

  echo ("Verifying " <> pack (show (Map.size db)) <> " packages.")
  echo "Warning: this could take some time!"

  let installOrUpdate' (name, pkgInfo) = (name, ) <$> installOrUpdate (set pkg) name pkgInfo
  paths <- Map.fromList <$> traverse installOrUpdate' (Map.toList db)

  for_ (Map.toList db) $ \(name, PackageInfo{..}) -> do
    let dirFor = fromMaybe (error "verifyPackageSet: no directory") . (`Map.lookup` paths)
    echo ("Verifying package " <> name)
    let srcGlobs = map (pathToTextUnsafe . (</> ("src" </> "**" </> "*.purs")) . dirFor) (name : dependencies)
    procs "psc" srcGlobs empty

main :: IO ()
main = do
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    cmd <- Opts.execParser opts
    cmd
  where
    opts        = Opts.info (versionInfo <*> Opts.helper <*> commands) infoModList
    infoModList = Opts.fullDesc <> headerInfo <> footerInfo
    headerInfo  = Opts.progDesc "Manage package dependencies"
    footerInfo  = Opts.footer $ "psc-package " ++ showVersion Paths.version

    versionInfo :: Parser (a -> a)
    versionInfo = Opts.abortOption (Opts.InfoMsg (showVersion Paths.version)) $
      Opts.long "version" <> Opts.help "Show the version number" <> Opts.hidden

    commands :: Parser (IO ())
    commands = (Opts.subparser . fold)
        [ Opts.command "init"
            (Opts.info (pure initialize)
            (Opts.progDesc "Initialize a new package"))
        , Opts.command "update"
            (Opts.info (pure update)
            (Opts.progDesc "Update dependencies"))
        , Opts.command "uninstall"
            (Opts.info (uninstall <$> pkg)
            (Opts.progDesc "Uninstall the named package"))
        , Opts.command "install"
            (Opts.info (install <$> pkg)
            (Opts.progDesc "Install the named package"))
        , Opts.command "build"
            (Opts.info (pure (exec "psc"))
            (Opts.progDesc "Build the current package and dependencies"))
        , Opts.command "dependencies"
            (Opts.info (pure listDependencies)
            (Opts.progDesc "List all (transitive) dependencies for the current package"))
        , Opts.command "sources"
            (Opts.info (pure listSourcePaths)
            (Opts.progDesc "List all (active) source paths for dependencies"))
        , Opts.command "available"
            (Opts.info (pure listPackages)
            (Opts.progDesc "List all packages available in the package set"))
        , Opts.command "updates"
            (Opts.info (checkForUpdates <$> apply <*> applyMajor)
            (Opts.progDesc "Check all packages in the package set for new releases"))
        , Opts.command "verify-set"
            (Opts.info (pure verifyPackageSet)
            (Opts.progDesc "Verify that the packages in the package set build correctly"))
        ]
      where
        pkg = Opts.strArgument $
             Opts.metavar "PACKAGE"
          <> Opts.help "The name of the package to install"

        apply = Opts.switch $
             Opts.long "apply"
          <> Opts.help "Apply all minor package updates"

        applyMajor = Opts.switch $
             Opts.long "apply-breaking"
          <> Opts.help "Apply all major package updates"
