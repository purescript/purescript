{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.PureScript.Make.Monad
  ( -- * Implementation of Make API using files on disk
    Make(..)
  , runMake
  , makeIO
  , getTimestamp
  , getTimestampMaybe
  , readTextFile
  , readJSONFile
  , readJSONFileIO
  , readStoreFile
  , readStoreFileIO
  , readCborFile
  , readCborFileIO
  , readExternsFile
  , readCborJsonFile
  , readCborJsonFileIO
  , hashFile
  , writeTextFile
  , writeStoreFile
  , writeJSONFile
  , writeCborFile
  , writeCborJsonFile
  , writeCborJsonFileIO
  , copyFile
  ) where

import           Prelude

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import qualified Codec.CBOR.Write as Write
import qualified Codec.CBOR.Read as Read
import qualified Codec.CBOR.JSON as CborJson
import           Control.Exception (tryJust)
import           Control.Monad (join, guard)
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader (MonadReader(..), ReaderT(..))
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Control.Monad.Trans.Except
import           Control.Monad.Writer.Class (MonadWriter(..))
import qualified Data.Aeson as Aeson
import qualified Data.Store as Store
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Language.PureScript.Errors
import           Language.PureScript.Externs (ExternsFile, externsIsCurrentVersion)
import           Language.PureScript.Make.Cache (ContentHash, hash)
import           Language.PureScript.Options
import           System.Directory (createDirectoryIfMissing, getModificationTime)
import qualified System.Directory as Directory
import           System.FilePath (takeDirectory)
import           System.IO.Error (tryIOError, isDoesNotExistError)
import           System.IO.UTF8 (readUTF8FileT)

-- | A monad for running make actions
newtype Make a = Make
  { unMake :: ReaderT Options (ExceptT MultipleErrors (Logger MultipleErrors)) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError MultipleErrors, MonadWriter MultipleErrors, MonadReader Options)

instance MonadBase IO Make where
  liftBase = liftIO

instance MonadBaseControl IO Make where
  type StM Make a = Either MultipleErrors a
  liftBaseWith f = Make $ liftBaseWith $ \q -> f (q . unMake)
  restoreM = Make . restoreM

-- | Execute a 'Make' monad, returning either errors, or the result of the compile plus any warnings.
runMake :: Options -> Make a -> IO (Either MultipleErrors a, MultipleErrors)
runMake opts = runLogger' . runExceptT . flip runReaderT opts . unMake

-- | Run an 'IO' action in the 'Make' monad. The 'String' argument should
-- describe what we were trying to do; it is used for rendering errors in the
-- case that an IOException is thrown.
makeIO :: (MonadIO m, MonadError MultipleErrors m) => Text -> IO a -> m a
makeIO description io = do
  res <- liftIO (tryIOError io)
  either (throwError . singleError . ErrorMessage [] . FileIOError description) pure res

-- | Get a file's modification time in the 'Make' monad, capturing any errors
-- using the 'MonadError' instance.
getTimestamp :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m UTCTime
getTimestamp path =
  makeIO ("get a timestamp for file: " <> Text.pack path) $ getModificationTime path

-- | Get a file's modification time in the 'Make' monad, returning Nothing if
-- the file does not exist.
getTimestampMaybe :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m (Maybe UTCTime)
getTimestampMaybe path =
  makeIO ("get a timestamp for file: " <> Text.pack path) $ catchDoesNotExist $ getModificationTime path

-- | Read a text file strictly in the 'Make' monad, capturing any errors using
-- the 'MonadError' instance.
readTextFile :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m Text
readTextFile path =
  makeIO ("read file: " <> Text.pack path) $
    readUTF8FileT path

-- | Read a JSON file in the 'Make' monad, returning 'Nothing' if the file does
-- not exist or could not be parsed. Errors are captured using the 'MonadError'
-- instance.
readJSONFile :: (MonadIO m, MonadError MultipleErrors m) => Aeson.FromJSON a => FilePath -> m (Maybe a)
readJSONFile path =
  makeIO ("read JSON file: " <> Text.pack path) (readJSONFileIO path)

readJSONFileIO :: Aeson.FromJSON a => FilePath -> IO (Maybe a)
readJSONFileIO path = do
  r <- catchDoesNotExist $ Aeson.decodeFileStrict' path
  return $ join r

readCborJsonFile :: (MonadIO m, MonadError MultipleErrors m) => Aeson.FromJSON a => FilePath -> m (Maybe a)
readCborJsonFile path =
  makeIO ("read JSON file: " <> Text.pack path) (readCborJsonFileIO path)

readCborJsonFileIO :: Aeson.FromJSON a => FilePath -> IO (Maybe a)
readCborJsonFileIO path = do
    r <- catchDoesNotExist $ LB.readFile path
    case r of
      Nothing -> pure Nothing
      Just bytes -> case Read.deserialiseFromBytes (CborJson.decodeValue False) bytes of
        Left err -> pure Nothing
        Right (leftover, v) -> pure $ case Aeson.fromJSON v of
          Aeson.Error _ -> Nothing
          Aeson.Success a -> Just a

-- | Read a Store encoded file in the 'Make' monad, returning
-- 'Nothing' if the file does not exist or could not be parsed. Errors
-- are captured using the 'MonadError' instance.
readStoreFile :: (MonadIO m, MonadError MultipleErrors m) => Store.Store a => FilePath -> m (Maybe a)
readStoreFile path =
  makeIO ("read Binary file: " <> Text.pack path) (readStoreFileIO path)

readStoreFileIO :: Store.Store a => FilePath -> IO (Maybe a)
readStoreFileIO path = do
  r <- catchDoesNotExist $ Store.decodeEx <$> B.readFile path
  return $ join r

-- | Read a Cbor encoded file in the 'Make' monad, returning
-- 'Nothing' if the file does not exist or could not be parsed. Errors
-- are captured using the 'MonadError' instance.
readCborFile :: (MonadIO m, MonadError MultipleErrors m) => Serialise a => FilePath -> m (Maybe a)
readCborFile path =
  makeIO ("read Binary file: " <> Text.pack path) (readCborFileIO path)

readCborFileIO :: Serialise a => FilePath -> IO (Maybe a)
readCborFileIO path = do
  r <- catchDoesNotExist $ Serialise.readFileDeserialise path
  return $ join r

-- | Read an externs file, returning 'Nothing' if the file does not exist,
-- could not be parsed, or was generated by a different version of the
-- compiler.
readExternsFile :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m (Maybe ExternsFile)
readExternsFile path = do
  -- rofl
  -- mexterns <- readJsonFile path
  -- mexterns <- readStoreFile path
  -- mexterns <- readCborJsonFile path
  mexterns <- readCborFile path
  return $ do
    externs <- mexterns
    guard $ externsIsCurrentVersion externs
    return externs

hashFile :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> m ContentHash
hashFile path = do
  makeIO ("hash file: " <> Text.pack path)
    (hash <$> B.readFile path)

-- | If the provided action threw an 'isDoesNotExist' error, catch it and
-- return Nothing. Otherwise return Just the result of the inner action.
catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist inner = do
  r <- tryJust (guard . isDoesNotExistError) inner
  case r of
    Left () ->
      return Nothing
    Right x ->
      return (Just x)

-- | Write a text file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
writeTextFile :: FilePath -> B.ByteString -> Make ()
writeTextFile path text = makeIO ("write file: " <> Text.pack path) $ do
  createParentDirectory path
  B.writeFile path text

-- | Write a JSON file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
writeJSONFile :: (MonadIO m, MonadError MultipleErrors m) => Aeson.ToJSON a => FilePath -> a -> m ()
writeJSONFile path value = makeIO ("write JSON file: " <> Text.pack path) $ do
  createParentDirectory path
  Aeson.encodeFile path value

-- | Write a Store encoded file in the 'Make' monad, capturing any
-- errors using the 'MonadError' instance.
writeStoreFile :: (MonadIO m, MonadError MultipleErrors m) => Store.Store a => FilePath -> a -> m ()
writeStoreFile path value = makeIO ("write Store file: " <> Text.pack path) $ do
  createParentDirectory path
  B.writeFile path (Store.encode value)

writeCborFile :: (MonadIO m, MonadError MultipleErrors m) => Serialise a => FilePath -> a -> m ()
writeCborFile path value = makeIO ("write Cbor file: " <> Text.pack path) $ do
  createParentDirectory path
  Serialise.writeFileSerialise path value

-- | Write a Store encoded file in the 'Make' monad, capturing any
-- errors using the 'MonadError' instance.
writeCborJsonFile :: (MonadIO m, MonadError MultipleErrors m) => Aeson.ToJSON a => FilePath -> a -> m ()
writeCborJsonFile path value =
  makeIO
    ("write Cborg Json file: " <> Text.pack path)
    (writeCborJsonFileIO path value)

writeCborJsonFileIO :: Aeson.ToJSON a => FilePath -> a -> IO ()
writeCborJsonFileIO path value = do
  createParentDirectory path
  B.writeFile path (Write.toStrictByteString (CborJson.encodeValue (Aeson.toJSON value)))

-- | Copy a file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
copyFile :: (MonadIO m, MonadError MultipleErrors m) => FilePath -> FilePath -> m ()
copyFile src dest =
  makeIO ("copy file: " <> Text.pack src <> " -> " <> Text.pack dest) $ do
    createParentDirectory dest
    Directory.copyFile src dest

createParentDirectory :: FilePath -> IO ()
createParentDirectory = createDirectoryIfMissing True . takeDirectory
