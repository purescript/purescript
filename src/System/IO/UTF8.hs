module System.IO.UTF8 where

import Prelude.Compat

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE

readUTF8FileT :: FilePath -> IO Text
readUTF8FileT inFile =
  fmap TE.decodeUtf8 (BS.readFile inFile)

writeUTF8FileT :: FilePath -> Text -> IO ()
writeUTF8FileT inFile text =
  BS.writeFile inFile (TE.encodeUtf8 text)

readUTF8File :: FilePath -> IO String
readUTF8File inFile =
  fmap UTF8.toString (BS.readFile inFile)

writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File inFile text =
  BS.writeFile inFile (UTF8.fromString text)
