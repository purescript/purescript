module System.IO.UTF8 where

import Prelude.Compat

import System.IO ( IOMode(..)
                 , hGetContents
                 , hSetEncoding
                 , hClose
                 , hPutStr
                 , openFile
                 , utf8
                 )

readUTF8File :: FilePath -> IO String
readUTF8File inFile = do
  h <- openFile inFile ReadMode
  hSetEncoding h utf8
  hGetContents h

writeUTF8File :: FilePath -> String -> IO ()
writeUTF8File inFile text = do
  h <- openFile inFile WriteMode
  hSetEncoding h utf8
  hPutStr h text
  hClose h
