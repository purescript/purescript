module System.IO.UTF8 where

import PSPrelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Search as BSS
import qualified Data.ByteString.UTF8 as UTF8
import Data.String (String)

-- | Unfortunately ByteString's readFile does not convert line endings on
-- Windows, so we have to do it ourselves
fixCRLF :: BS.ByteString -> BS.ByteString
fixCRLF = BSL.toStrict . BSS.replace "\r\n" ("\n" :: BS.ByteString)

readUTF8FileT :: FilePath -> IO Text
readUTF8FileT inFile =
  fmap (decodeUtf8 . fixCRLF) (BS.readFile inFile)

writeUTF8FileT :: FilePath -> Text -> IO ()
writeUTF8FileT inFile text =
  BS.writeFile inFile (encodeUtf8 text)

readUTF8File :: FilePath -> IO String
readUTF8File inFile =
  fmap (UTF8.toString . fixCRLF) (BS.readFile inFile)

writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File inFile text =
  BS.writeFile inFile (UTF8.fromString text)
