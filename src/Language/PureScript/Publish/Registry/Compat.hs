-- | A compatibility module that allows a restricted set of purs.json manifest
-- | files to be used for publishing. The manifest must described a package
-- | available on GitHub, and it must be convertable to a Bower manifest.
-- |
-- | Fully supporting the registry manifest format will require `purs publish`
-- | and by extension Pursuit to relax the requirement that packages are hosted
-- | on GitHub, because the registry does not have this requirement.
module Language.PureScript.Publish.Registry.Compat where

import Protolude
import Data.Map qualified as Map
import Web.Bower.PackageMeta qualified as Bower
import Data.Bitraversable (Bitraversable(..))
import Data.Aeson.BetterErrors (key, asText, keyMay, eachInObject, Parse, throwCustomError)

-- | Convert a valid purs.json manifest into a bower.json manifest
toBowerPackage :: PursJson -> Either Bower.BowerError Bower.PackageMeta
toBowerPackage PursJson{..} = do
  bowerName <- Bower.parsePackageName ("purescript-" <> pursJsonName)
  let
    bowerDescription = pursJsonDescription
    bowerMain = []
    bowerModuleType = []
    bowerLicense = [ pursJsonLicense ]
    bowerIgnore = []
    bowerKeywords = []
    bowerAuthors = []
    bowerHomepage = Just pursJsonLocation
    bowerRepository = Just $ Bower.Repository { repositoryUrl = pursJsonLocation, repositoryType = "git" }
    bowerDevDependencies = []
    bowerResolutions = []
    bowerPrivate = False

  let parseDependencies = traverse (bitraverse (Bower.parsePackageName . ("purescript-" <>)) (pure . Bower.VersionRange))
  bowerDependencies <- parseDependencies $ Map.toAscList pursJsonDependencies
  pure $ Bower.PackageMeta {..}

-- | A partial representation of the purs.json manifest format, including only
-- | the fields required for publishing.
-- |
-- | https://github.com/purescript/registry/blob/master/v1/Manifest.dhall
--
-- This type is intended for compatibility with the Bower publishing pipeline,
-- and does not accurately reflect all possible purs.json manifests. However,
-- supporting purs.json manifests properly introduces breaking changes to the
-- compiler and to Pursuit.
data PursJson = PursJson
  { -- | The name of the package
    pursJsonName :: Text
    -- | The SPDX identifier representing the package license
  , pursJsonLicense :: Text
    -- | The GitHub repository hosting the package
  , pursJsonLocation :: Text
    -- | An optional description of the package
  , pursJsonDescription :: Maybe Text
    -- | A map of dependencies, where keys are package names and values are
    -- | dependency ranges of the form '>=X.Y.Z <X.Y.Z'
  , pursJsonDependencies :: Map Text Text
  }

data PursJsonError
  = MalformedLocationField
  deriving (Eq, Show, Ord, Generic)

instance NFData PursJsonError

showPursJsonError :: PursJsonError -> Text
showPursJsonError = \case
  MalformedLocationField ->
    "The 'location' field must be either '{ \"githubOwner\": OWNER, \"githubRepo\": REPO }' or '{ \"gitUrl\": URL }'."

asPursJson :: Parse PursJsonError PursJson
asPursJson = do
  pursJsonName <- key "name" asText
  pursJsonDescription <- keyMay "description" asText
  pursJsonLicense <- key "license" asText
  pursJsonDependencies <- key "dependencies" (Map.fromAscList <$> eachInObject asText)
  -- Packages are required to come from GitHub in PureScript 0.14.x, but the
  -- PureScript registry does not require this, nor does it require that
  -- packages are Git repositories. This restriction should be lifted when
  -- we fully support purs.json manifests in the compiler and on Pursuit.
  --
  -- For the time being, we only parse manifests that include a GitHub owner
  -- and repo pair, or which specify a Git URL, which we use to try and get
  -- the package from GitHub.
  pursJsonLocation <- key "location" asOwnerRepoOrGitUrl
  pure $ PursJson{..}
  where
  asOwnerRepoOrGitUrl =
    catchError asOwnerRepo (\_ -> catchError asGitUrl (\_ -> throwCustomError MalformedLocationField))

  asGitUrl =
    key "gitUrl" asText

  asOwnerRepo = do
    githubOwner <- key "githubOwner" asText
    githubRepo <- key "githubRepo" asText
    pure $ "https://github.com/" <> githubOwner <> "/" <> githubRepo <> ".git"
