{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Control.Foldl as Foldl
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson
import           Data.Foldable (fold, for_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (pack)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Encoding (encodeUtf8)
import           Data.Traversable (for)
import           Data.Version (showVersion)
import qualified Filesystem.Path.CurrentOS as Path
import           GHC.Generics (Generic)
import qualified Options.Applicative as Opts
import qualified Paths_purescript as Paths
import qualified System.IO as IO
import           Turtle hiding (fold)
import qualified Turtle.Shell as Shell

packageFile :: Path.FilePath
packageFile = "psc-package.json"

data PackageConfig = PackageConfig
  { name    :: Text
  , depends :: [Text]
  } deriving (Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

defaultPackage :: Text -> PackageConfig
defaultPackage pkgName = PackageConfig pkgName [ "prelude" ]

data PackageInfo = PackageInfo
  { repo         :: Text
  , version      :: Text
  , dependencies :: [Text]
  } deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

type PackageSet = Map.Map Text PackageInfo

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
      [ Opts.command "init"    (Opts.info (pure initialize) (Opts.progDesc "Initialize a new package"))
      , Opts.command "update"  (Opts.info (pure update)     (Opts.progDesc "Update dependencies"))
      , Opts.command "build"   (Opts.info (pure build)      (Opts.progDesc "Build the current package and dependencies"))
      ]

initialize :: IO ()
initialize = do
  here <- pwd
  isEmpty <- Shell.fold (ls here) Foldl.null
  unless isEmpty $ do
    echo "Current directory is not empty"
    exit (ExitFailure 1)
  echo "Initializing new project in current directory"
  let pkgName = either (const "new-package") id (Path.toText (Path.filename here))
      pkg = defaultPackage pkgName
  writeTextFile packageFile (toStrict (Aeson.encodeToLazyText pkg))
  updateImpl pkg

update :: IO ()
update = do
  exists <- testfile packageFile
  unless exists $ do
    echo "psc-package.json does not exist"
    exit (ExitFailure 2)
  mpkg <- Aeson.decodeStrict . encodeUtf8 <$> readTextFile packageFile
  case mpkg of
    Nothing -> do
      echo "Unable to parse psc-package.json"
      exit (ExitFailure 3)
    Just pkg -> updateImpl pkg

getPackageSet :: IO ()
getPackageSet = do
  let pkgDir = ".psc-package" </> ".package-set"
  exists <- testdir pkgDir
  if exists
    then
      pushd pkgDir $
        procs "git" [ "pull", "origin", "master" ] empty
    else
      procs "git"
            [ "clone"
            , "--depth", "1"
            , "https://github.com/paf31/purescript-package-db.git"
            , (either (error "Path.toText failed") id . Path.toText) pkgDir
            ] empty

readPackageSet :: IO PackageSet
readPackageSet = do
  let dbFile = ".psc-package" </> ".package-set" </> "packages.json"
  exists <- testfile dbFile
  unless exists $ do
    echo "packages.json does not exist"
    exit (ExitFailure 4)
  mdb <- Aeson.decodeStrict . encodeUtf8 <$> readTextFile dbFile
  case mdb of
    Nothing -> do
      echo "Unable to parse packages.json"
      exit (ExitFailure 5)
    Just db -> return db

installOrUpdate :: Text -> PackageInfo -> IO ()
installOrUpdate pkgName PackageInfo{..} = do
  let pkgDir = ".psc-package" </> ".packages" </> fromText pkgName
  exists <- testdir pkgDir
  if exists
    then
      pushd pkgDir $
        procs "git"
              [ "checkout"
              , version
              ] empty
    else
      procs "git"
            [ "clone"
            , "-b", version
            , "--depth", "1"
            , repo
            , (either (error "Path.toText failed") id . Path.toText) pkgDir
            ] empty

pushd :: Path.FilePath -> IO a -> IO a
pushd dir action = do
  cur <- pwd
  cd dir
  a <- action
  cd cur
  return a

updateImpl :: PackageConfig -> IO ()
updateImpl PackageConfig{..} = do
  getPackageSet
  db <- readPackageSet
  pkgs <- for depends $ \pkg ->
    case Map.lookup pkg db of
      Nothing -> do
        echo ("Package " <> pkg <> " does not exist in package set")
        exit (ExitFailure 6)
      Just PackageInfo{..} -> return (pkg : dependencies)
  let allDeps = foldMap Set.fromList pkgs
  echo ("Updating " <> pack (show (Set.size allDeps)) <> " packages")
  for_ allDeps $ \pkgName ->
    for_ (Map.lookup pkgName db) $ \pkg ->
      installOrUpdate pkgName pkg

build :: IO ()
build = do
  procs "psc"
        [ "src/**/*.purs"
        , ".psc-package/.packages/*/src/**/*.purs"
        ] empty
