module System.IO.UTF8 where

import Prelude.Compat

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Search as BSS
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- | Unfortunately ByteString's readFile does not convert line endings on
-- Windows, so we have to do it ourselves
fixCRLF :: BS.ByteString -> BS.ByteString
fixCRLF = BSL.toStrict . BSS.replace "\r\n" ("\n" :: BS.ByteString)

readUTF8FileT :: FilePath -> IO Text
readUTF8FileT inFile =
  fmap (TE.decodeUtf8 . fixCRLF) (BS.readFile inFile)

writeUTF8FileT :: FilePath -> Text -> IO ()
writeUTF8FileT inFile text =
  BS.writeFile inFile (TE.encodeUtf8 text)

readUTF8File :: FilePath -> IO String
readUTF8File inFile =
  fmap (UTF8.toString . fixCRLF) (BS.readFile inFile)

writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File inFile text =
  BS.writeFile inFile (UTF8.fromString text)
