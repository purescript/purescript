module Language.PureScript.Publish.Registry where

import Protolude
import Data.Aeson (FromJSON(..), (.:), withObject, Object)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A partial representation of the .purs.json manifest format, including only
-- | the fields required for publishing.
-- |
-- | https://github.com/purescript/registry/blob/master/v1/Manifest.dhall
data PursJson = PursJson
  { -- | The name of the package
    pursJsonName :: Text
    -- | The license of the package, which must be a valid SPDX identifier
  , pursJsonLicense :: Text
    -- | The registry-compliant version of the package, which is SemVer minus
    -- | build metadata and prerelease identifiers. This version can be used
    -- | to associate this manifest with its metadata.
  , pursJsonVersion :: Text
    -- | A map of dependencies, where keys are package names and values are
    -- | dependency ranges of the form '>=X.Y.Z <X.Y.Z'
  , pursJsonDependencies :: Map Text Text
  }

instance FromJSON PursJson where
  parseJSON = withObject "PursJson" $ \obj -> do
    pursJsonName <- obj .: "name"
    pursJsonLicense <- obj .: "license"
    pursJsonVersion <- obj .: "version"
    pursJsonDependencies <- obj .: "dependencies"
    pure $ PursJson {..}

-- | A partial representation of the registry metadata stored for a package,
-- | including only the fields required for publishing.
-- |
-- | https://github.com/purescript/registry/blob/master/v1/Metadata.dhall
data PackageMetadata = PackageMetadata
  { -- | A set of package versions that have been released
    packageMetadataReleases :: Set Text
    -- | A set of package versions that were released, and then were unpublished
  , packageMetadataUnpublished :: Set Text
  }

instance FromJSON PackageMetadata  where
  parseJSON = withObject "PackageMetadata" $ \obj -> do
    let toSet = Set.fromList . Map.keys
    releases :: Map Text Object <- obj .: "releases"
    unpublished :: Map Text Text <- obj .: "unpublished"
    let packageMetadataReleases = toSet releases
    let packageMetadataUnpublished = toSet unpublished
    pure $ PackageMetadata {..}
