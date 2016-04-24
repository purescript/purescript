-- |
-- A small script which regenerates the LICENSE file with all
-- dependencies' licenses, when the dependencies are provided via standard
-- input.
--
-- It is recommended to run this as follows:
--
-- stack list-dependencies | cut -f 1 -d ' ' | stack exec runhaskell license-generator/generate.hs > LICENSE
--

module Main (main) where

import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.List
import System.Process
import System.IO (hPutStrLn, stderr, getContents)

echoHeader :: IO ()
echoHeader =
    readFile "license-generator/header.txt" >>= putStr

depsNames :: IO [String]
depsNames =
    fmap (filter (/= "purescript") . lines) getContents

depsLicense :: String -> IO ()
depsLicense dep = do
    let licenseFile = if dep == "Glob" then "LICENSE.txt" else "LICENSE"
    hPutStrLn stderr dep
    license <- readProcess "curl" ["--silent", "https://hackage.haskell.org/package/" ++ dep ++ "/src/" ++ licenseFile] ""
    putStrLn $ dep ++ " LICENSE file:"
    putStrLn ""
    putStrLn $ f license
  where
    f = unlines . map (trimEnd . ("  " ++)) . lines
    trimEnd = reverse . dropWhile isSpace . reverse

main :: IO ()
main = do
    deps <-depsNames
    echoHeader
    putStrLn ""
    forM_ deps $ \d -> putStr "  " >> putStrLn d
    putStrLn ""
    forM_ deps depsLicense
