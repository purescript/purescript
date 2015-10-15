module System.IO.UTF8
where
import System.IO (hGetContents, hSetEncoding, openFile, utf8, IOMode (..))

readUTF8File :: FilePath -> IO String
readUTF8File inFile = do
    h <- openFile inFile ReadMode
    hSetEncoding h utf8
    hGetContents h
