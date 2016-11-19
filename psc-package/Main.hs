{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Aeson as Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Foldable (fold, for_, traverse_)
import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.Text (pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           Data.Text.Encoding (encodeUtf8)
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
    echo "packages.json does not exist"
    exit (ExitFailure 1)
  mdb <- Aeson.decodeStrict . encodeUtf8 <$> readTextFile dbFile
  case mdb of
    Nothing -> do
      echo "Unable to parse packages.json"
      exit (ExitFailure 1)
    Just db -> return db

installOrUpdate :: PackageConfig -> Text -> PackageInfo -> IO ()
installOrUpdate PackageConfig{ set } pkgName PackageInfo{ repo, version } = do
  let pkgDir = ".psc-package" </> fromText set </> fromText pkgName </> fromText version
  exists <- testdir pkgDir
  unless exists . void $ cloneShallow repo version pkgDir

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
