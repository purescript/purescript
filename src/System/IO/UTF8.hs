module System.IO.UTF8 where

import Prelude.Compat

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8

readUTF8File :: FilePath -> IO String
readUTF8File inFile = do
  fmap UTF8.toString (BS.readFile inFile)

writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File inFile text = do
  BS.writeFile inFile (UTF8.fromString text)
