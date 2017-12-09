module System.IO.UTF8 where

import PSPrelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as LT
import Data.String (String)
import System.IO as IO


readUTF8FileT :: FilePath -> IO Text
readUTF8FileT = fmap toS . readUTF8FileBS

writeUTF8FileT :: FilePath -> Text -> IO ()
writeUTF8FileT f = writeUTF8FileBSL f . toS

readUTF8FileTL :: FilePath -> IO LT.Text
readUTF8FileTL = fmap toS . readUTF8FileBS

writeUTF8FileTL :: FilePath -> LText -> IO ()
writeUTF8FileTL f = writeUTF8FileBSL f . toS

readUTF8FileS :: FilePath -> IO String
readUTF8FileS = fmap toS . readUTF8FileBS

writeUTF8FileS :: FilePath -> String -> IO ()
writeUTF8FileS f = writeUTF8FileBSL f . toS

readUTF8FileBS :: FilePath -> IO BS.ByteString
readUTF8FileBS f = bracket open hClose BS.hGetContents
  where open = do
          h <- IO.openFile f ReadMode
          IO.hSetEncoding h IO.utf8
          IO.hSetNewlineMode h IO.universalNewlineMode
          pure h

writeUTF8FileBSL :: FilePath -> BSL.ByteString -> IO ()
writeUTF8FileBSL f s = bracket open hClose (flip BSL.hPut s)
  where open = do
          h <- IO.openFile f WriteMode
          IO.hSetEncoding h IO.utf8
          IO.hSetNewlineMode h IO.noNewlineTranslation
          pure h

withUTF8FileContentsT :: MonadIO m => FilePath -> (Text -> a) -> m a
withUTF8FileContentsT f op = withUTF8FileContents' f (op . toS)

withUTF8FileContentsTL :: MonadIO m => FilePath -> (LT.Text -> a) -> m a
withUTF8FileContentsTL f op = withUTF8FileContents' f (op . toS)

withUTF8FileContents :: MonadIO m => FilePath -> (String -> a) -> m a
withUTF8FileContents f op = withUTF8FileContents' f (op . toS)

withUTF8FileContents' :: MonadIO m => FilePath -> (BS.ByteString -> a) -> m a
withUTF8FileContents' = runWithUTF8FileContentsBS pure

runWithUTF8FileContentsT :: MonadIO m => (b -> IO a) -> FilePath -> (Text -> b) -> m a
runWithUTF8FileContentsT runner f op = runWithUTF8FileContentsBS runner f (op . toS)

runWithUTF8FileContentsBS :: MonadIO m => (b -> IO a) -> FilePath -> (BS.ByteString -> b) -> m a
runWithUTF8FileContentsBS runner f op = liftIO $ readUTF8FileBS f >>= runner . op
