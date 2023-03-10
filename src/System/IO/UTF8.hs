module System.IO.UTF8 where

import Prelude

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Search qualified as BSS
import Data.ByteString.UTF8 qualified as UTF8
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Protolude (ordNub)

-- | Unfortunately ByteString's readFile does not convert line endings on
-- Windows, so we have to do it ourselves
fixCRLF :: BS.ByteString -> BS.ByteString
fixCRLF = BSL.toStrict . BSS.replace "\r\n" ("\n" :: BS.ByteString)

readUTF8FilesT :: [FilePath] -> IO [(FilePath, Text)]
readUTF8FilesT =
  traverse (\inFile -> (inFile, ) <$> readUTF8FileT inFile) . ordNub

readUTF8FileT :: FilePath -> IO Text
readUTF8FileT inFile =
  fmap (TE.decodeUtf8 . fixCRLF) (BS.readFile inFile)

writeUTF8FileT :: FilePath -> Text -> IO ()
writeUTF8FileT inFile text =
  BS.writeFile inFile (TE.encodeUtf8 text)

readUTF8File :: FilePath -> IO String
readUTF8File inFile =
  fmap (UTF8.toString . fixCRLF) (BS.readFile inFile)
