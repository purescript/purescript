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
  , readTextFileMaybe
  , readJSONFile
  , writeTextFile
  , writeJSONFile
  ) where

import           Prelude

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
import qualified Data.ByteString.Lazy as B
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.Clock (UTCTime)
import           Language.PureScript.AST
import           Language.PureScript.Errors
import           Language.PureScript.Options
import           System.Directory (createDirectoryIfMissing, getModificationTime)
import           System.FilePath (takeDirectory)
import           System.IO.Error (tryIOError, isDoesNotExistError)

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
makeIO :: Text -> IO a -> Make a
makeIO description io = do
  e <- liftIO $ tryIOError io
  either (throwError . singleError . ErrorMessage [] . FileIOError description) return e

-- | Get a file's modification time in the 'Make' monad, capturing any errors
-- using the 'MonadError' instance.
getTimestamp :: FilePath -> Make UTCTime
getTimestamp path =
  makeIO ("get a timestamp for file: " <> Text.pack path) $ getModificationTime path

-- | Get a file's modification time in the 'Make' monad, returning Nothing if
-- the file does not exist.
getTimestampMaybe :: FilePath -> Make (Maybe UTCTime)
getTimestampMaybe path =
  makeIO ("get a timestamp for file: " <> Text.pack path) $ catchDoesNotExist $ getModificationTime path

-- | Read a text file in the 'Make' monad, capturing any errors using the
-- 'MonadError' instance.
readTextFile :: FilePath -> Make B.ByteString
readTextFile path =
  makeIO ("read file: " <> Text.pack path) $ B.readFile path

-- | Read a text file in the 'Make' monad, or return 'Nothing' if the file does
-- not exist. Errors are captured using the 'MonadError' instance.
readTextFileMaybe :: FilePath -> Make (Maybe B.ByteString)
readTextFileMaybe path =
  makeIO ("read file: " <> Text.pack path) $ catchDoesNotExist $ B.readFile path

-- | Read a JSON file in the 'Make' monad, returning 'Nothing' if the file does
-- not exist or could not be parsed. Errors are captured using the 'MonadError'
-- instance.
readJSONFile :: Aeson.FromJSON a => FilePath -> Make (Maybe a)
readJSONFile path =
  makeIO ("read JSON file: " <> Text.pack path) $ do
    r <- catchDoesNotExist $ Aeson.decodeFileStrict' path
    return $ join r

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
writeJSONFile :: Aeson.ToJSON a => FilePath -> a -> Make ()
writeJSONFile path value = makeIO ("write JSON file: " <> Text.pack path) $ do
  createParentDirectory path
  Aeson.encodeFile path value

createParentDirectory :: FilePath -> IO ()
createParentDirectory = createDirectoryIfMissing True . takeDirectory
