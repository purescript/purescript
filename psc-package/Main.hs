{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Control.Applicative (liftA3)
import           Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Align.Key (alignWithKey)
import           Data.Foldable (fold, for_, traverse_)
import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import           Data.Text (pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           Data.Text.Encoding (encodeUtf8)
import           Data.These (These(..))
import           Data.Traversable (for)
import           Data.Version (showVersion)
import qualified Filesystem.Path.CurrentOS as Path
import           GHC.Generics (Generic)
import qualified Options.Applicative as Opts
import qualified Paths_purescript as Paths
import qualified System.IO as IO
import           Turtle hiding (fold)

packageFile :: Path.FilePath
packageFile = "psc-package.json"

data PackageInfo = PackageInfo
  { packageInfoRepo         :: Text
  , packageInfoVersion      :: Text
  , packageInfoDependencies :: [Text]
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON PackageInfo where
  toJSON PackageInfo{..} =
    Aeson.object [ "repo"         .= packageInfoRepo
                 , "version"      .= packageInfoVersion
                 , "dependencies" .= packageInfoDependencies
                 ]

instance Aeson.FromJSON PackageInfo where
  parseJSON (Aeson.Object v) =
    PackageInfo <$> v .: "repo"
                <*> v .: "version"
                <*> v .: "dependencies"
  parseJSON _ = empty

type PackageSet = Map.Map Text PackageInfo

data Override = Override
  { overrideRepo         :: Maybe Text
  , overrideVersion      :: Maybe Text
  , overrideDependencies :: Maybe [Text]
  } deriving (Show, Eq, Generic)

instance Aeson.ToJSON Override where
  toJSON Override{..} =
    Aeson.object [ "repo"         .= overrideRepo
                 , "version"      .= overrideVersion
                 , "dependencies" .= overrideDependencies
                 ]

instance Aeson.FromJSON Override where
  parseJSON (Aeson.Object v) =
    Override <$> v .:? "repo"
             <*> v .:? "version"
             <*> v .:? "dependencies"
  parseJSON _ = empty

type Overrides = Map.Map Text Override

data PackageConfig = PackageConfig
  { packageConfigName         :: Text
  , packageConfigDependencies :: [Text]
  , packageConfigSet          :: Text
  , packageConfigSource       :: Text
  , packageConfigOverrides    :: Maybe Overrides
  } deriving (Show, Generic)

instance Aeson.ToJSON PackageConfig where
  toJSON PackageConfig{..} =
    Aeson.object [ "name"      .= packageConfigName
                 , "depends"   .= packageConfigDependencies
                 , "set"       .= packageConfigSet
                 , "source"    .= packageConfigSource
                 , "overrides" .= packageConfigOverrides
                 ]

instance Aeson.FromJSON PackageConfig where
  parseJSON (Aeson.Object v) =
    PackageConfig <$> v .:  "name"
                  <*> v .:  "depends"
                  <*> v .:  "set"
                  <*> v .:  "source"
                  <*> v .:? "overrides"
  parseJSON _ = empty

pathToTextUnsafe :: Turtle.FilePath -> Text
pathToTextUnsafe = either (error "Path.toText failed") id . Path.toText

defaultPackage :: Text -> PackageConfig
defaultPackage pkgName =
  PackageConfig { packageConfigName         = pkgName
                , packageConfigDependencies = [ "prelude" ]
                , packageConfigSet          = "psc-" <> pack (showVersion Paths.version)
                , packageConfigSource       = "https://github.com/purescript/package-sets.git"
                , packageConfigOverrides    = Nothing
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

encodePrettyToText :: Aeson.ToJSON json => json -> Text
encodePrettyToText =
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

writePackageFile :: PackageConfig -> IO ()
writePackageFile =
  writeTextFile packageFile
  . encodePrettyToText

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

getPackageSet :: PackageConfig -> IO ()
getPackageSet PackageConfig{ packageConfigSource, packageConfigSet } = do
  let pkgDir = ".psc-package" </> fromText packageConfigSet
                              </> ".set"
  exists <- testdir pkgDir
  unless exists . void $ cloneShallow packageConfigSource packageConfigSet pkgDir

readPackageSet :: PackageConfig -> IO PackageSet
readPackageSet PackageConfig{ packageConfigOverrides, packageConfigSet } = do
  let dbFile = ".psc-package" </> fromText packageConfigSet
                              </> ".set"
                              </> "packages.json"
  exists <- testfile dbFile
  unless exists $ do
    echo "packages.json does not exist"
    exit (ExitFailure 1)
  mdb <- Aeson.decodeStrict . encodeUtf8 <$> readTextFile dbFile
  case mdb of
    Nothing -> do
      echo "Unable to parse packages.json"
      exit (ExitFailure 1)
    Just db ->
      case sequence (alignWithKey applyOverride (fold packageConfigOverrides) db) of
        Left msg -> do
          echo msg
          exit (ExitFailure 1)
        Right db' -> return db'

applyOverride :: Text -> These Override PackageInfo -> Either Text PackageInfo
applyOverride pkgName (This Override{..}) =
  case liftA3 PackageInfo overrideRepo overrideVersion overrideDependencies of
    Just result -> Right result
    Nothing -> Left ("Package " <> pkgName <> " was not present in the package set. Please specify its overrides completely.")
applyOverride _ (That pkg) = pure pkg
applyOverride _ (These Override{..} PackageInfo{..}) = pure
  PackageInfo { packageInfoRepo         = fromMaybe packageInfoRepo overrideRepo
              , packageInfoVersion      = fromMaybe packageInfoVersion overrideVersion
              , packageInfoDependencies = fromMaybe packageInfoDependencies overrideDependencies
              }

installOrUpdate :: PackageConfig -> Text -> PackageInfo -> IO ()
installOrUpdate PackageConfig{ packageConfigSet } pkgName PackageInfo{ packageInfoRepo, packageInfoVersion } = do
  let pkgDir = ".psc-package" </> fromText packageConfigSet
                              </> fromText pkgName
                              </> fromText packageInfoVersion
  exists <- testdir pkgDir
  unless exists . void $ cloneShallow packageInfoRepo packageInfoVersion pkgDir

getTransitiveDeps :: PackageSet -> [Text] -> IO [(Text, PackageInfo)]
getTransitiveDeps db depends = do
  pkgs <- for depends $ \pkg ->
    case Map.lookup pkg db of
      Nothing -> do
        echo ("Package " <> pkg <> " does not exist in package set")
        exit (ExitFailure 1)
      Just PackageInfo{ packageInfoDependencies } -> return (pkg : packageInfoDependencies)
  let unique = Set.toList (foldMap Set.fromList pkgs)
  return (mapMaybe (\name -> fmap (name, ) (Map.lookup name db)) unique)

updateImpl :: PackageConfig -> IO ()
updateImpl config@PackageConfig{ packageConfigDependencies } = do
  getPackageSet config
  db <- readPackageSet config
  trans <- getTransitiveDeps db packageConfigDependencies
  echo ("Updating " <> pack (show (length trans)) <> " packages...")
  for_ trans $ \(pkgName, pkg) -> do
    echo ("Updating " <> pkgName)
    installOrUpdate config pkgName pkg

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
  let pkg' = pkg { packageConfigDependencies = nub (pack pkgName : packageConfigDependencies pkg) }
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
  pkg@PackageConfig{ packageConfigDependencies } <- readPackageFile
  db <- readPackageSet pkg
  trans <- getTransitiveDeps db packageConfigDependencies
  traverse_ (echo . fst) trans

listPackages :: IO ()
listPackages = do
    pkg <- readPackageFile
    db <- readPackageSet pkg
    traverse_ echo (fmt <$> Map.assocs db)
  where
    fmt :: (Text, PackageInfo) -> Text
    fmt (name, PackageInfo{ packageInfoVersion }) = name <> " (" <> packageInfoVersion <> ")"

getSourcePaths :: PackageConfig -> PackageSet -> [Text] -> IO [Turtle.FilePath]
getSourcePaths PackageConfig{..} db pkgNames = do
  trans <- getTransitiveDeps db pkgNames
  let paths = [ ".psc-package"
                </> fromText packageConfigSet
                </> fromText pkgName
                </> fromText packageInfoVersion
                </> "src" </> "**" </> "*.purs"
              | (pkgName, PackageInfo{ packageInfoVersion }) <- trans
              ]
  return paths

listSourcePaths :: IO ()
listSourcePaths = do
  pkg@PackageConfig{ packageConfigDependencies } <- readPackageFile
  db <- readPackageSet pkg
  paths <- getSourcePaths pkg db packageConfigDependencies
  traverse_ (echo . pathToTextUnsafe) paths

exec :: Text -> IO ()
exec exeName = do
  pkg@PackageConfig{..} <- readPackageFile
  db <- readPackageSet pkg
  paths <- getSourcePaths pkg db packageConfigDependencies
  procs exeName
        (map pathToTextUnsafe ("src" </> "**" </> "*.purs" : paths))
        empty

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
        ]
      where
        pkg = Opts.strArgument $
             Opts.metavar "PACKAGE"
          <> Opts.help "The name of the package to install"
