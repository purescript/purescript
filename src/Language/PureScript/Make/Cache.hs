{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.Make.Cache
  ( ContentHash
  , hash
  , CacheDb
  , CacheInfo(..)
  , checkChanged
  , removeModule
  , updateModule
  , normaliseForCache
  ) where

import Prelude

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Crypto.Hash (HashAlgorithm, Digest, SHA512)
import qualified Crypto.Hash as Hash
import qualified Data.Aeson as Aeson
import Data.Align (align)
import Data.ByteArray.Encoding (Base(Base16), convertToBase, convertFromBase)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (All(..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.These (These(..))
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import qualified System.FilePath as FilePath

import Language.PureScript.Names (ModuleName)

digestToHex :: Digest a -> Text
digestToHex = decodeUtf8 . convertToBase Base16

digestFromHex :: forall a. HashAlgorithm a => Text -> Maybe (Digest a)
digestFromHex =
  encodeUtf8
  >>> either (const Nothing) Just . convertFromBase Base16
  >=> (Hash.digestFromByteString :: BS.ByteString -> Maybe (Digest a))

-- | Defines the hash algorithm we use for cache invalidation of input files.
newtype ContentHash = ContentHash
  { unContentHash :: Digest SHA512 }
  deriving (Show, Eq, Ord)

instance Aeson.ToJSON ContentHash where
  toJSON = Aeson.toJSON . digestToHex . unContentHash

instance Aeson.FromJSON ContentHash where
  parseJSON x = do
    str <- Aeson.parseJSON x
    case digestFromHex str of
      Just digest ->
        pure $ ContentHash digest
      Nothing ->
        fail "Unable to decode ContentHash"

hash :: BS.ByteString -> ContentHash
hash = ContentHash . Hash.hash

type CacheDb = Map ModuleName CacheInfo

-- | A CacheInfo contains all of the information we need to store about a
-- particular module in the cache database.
newtype CacheInfo = CacheInfo
  { unCacheInfo :: Map FilePath (UTCTime, ContentHash) }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Semigroup, Monoid, Aeson.FromJSON, Aeson.ToJSON)

-- | Given a module name, and a map containing the associated input files
-- together with current metadata i.e. timestamps and hashes, check whether the
-- input files have changed, based on comparing with the database stored in the
-- monadic state.
--
-- The CacheInfo in the return value should be stored in the cache for future
-- builds.
--
-- The Bool in the return value indicates whether it is safe to use existing
-- build artifacts for this module, at least based on the timestamps and hashes
-- of the module's input files.
--
-- If the timestamps are the same as those in the database, assume the file is
-- unchanged, and return True without checking hashes.
--
-- If any of the timestamps differ from what is in the database, check the
-- hashes of those files. In this case, update the database with any changed
-- timestamps and hashes, and return True if and only if all of the hashes are
-- unchanged.
checkChanged
  :: Monad m
  => CacheDb
  -> ModuleName
  -> FilePath
  -> Map FilePath (UTCTime, m ContentHash)
  -> m (CacheInfo, Bool)
checkChanged cacheDb mn basePath currentInfo = do

  let dbInfo = unCacheInfo $ fromMaybe mempty (Map.lookup mn cacheDb)
  (newInfo, isUpToDate) <-
    fmap mconcat $
      for (Map.toList (align dbInfo currentInfo)) $ \(normaliseForCache basePath -> fp, aligned) -> do
        case aligned of
          This _ -> do
            -- One of the input files listed in the cache no longer exists;
            -- remove that file from the cache and note that the module needs
            -- rebuilding
            pure (Map.empty, All False)
          That (timestamp, getHash) -> do
            -- The module has a new input file; add it to the cache and
            -- note that the module needs rebuilding.
            newHash <- getHash
            pure (Map.singleton fp (timestamp, newHash), All False)
          These db@(dbTimestamp, _) (newTimestamp, _) | dbTimestamp == newTimestamp -> do
            -- This file exists both currently and in the cache database,
            -- and the timestamp is unchanged, so we skip checking the
            -- hash.
            pure (Map.singleton fp db, mempty)
          These (_, dbHash) (newTimestamp, getHash) -> do
            -- This file exists both currently and in the cache database,
            -- but the timestamp has changed, so we need to check the hash.
            newHash <- getHash
            pure (Map.singleton fp (newTimestamp, newHash), All (dbHash == newHash))

  pure (CacheInfo newInfo, getAll isUpToDate)

-- | Removes the given module from the cache database; used when
-- it failed to build.
removeModule :: ModuleName -> CacheDb -> CacheDb
removeModule = Map.delete

-- | Moves cache info between databases; used when a module was built successfully
updateModule :: ModuleName -> CacheDb -> CacheDb -> CacheDb
updateModule mn cur new =
  case Map.lookup mn new of
    Just r -> Map.insert mn r cur
    Nothing -> Map.delete mn cur

-- | 1. Any path that is beneath our current working directory will be
-- stored as a normalised relative path
-- 2. Any path that isn't will be stored as an absolute path
normaliseForCache :: FilePath -> FilePath -> FilePath
normaliseForCache basePath fp =
    if FilePath.isRelative fp then
      FilePath.normalise fp
    else
      let relativePath = FilePath.makeRelative basePath fp in
      if FilePath.isRelative relativePath then
        FilePath.normalise relativePath
      else
        -- If the path is still absolute after trying to make it
        -- relative to the base that means it is not underneath
        -- the base path
        FilePath.normalise fp
