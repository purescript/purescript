module Main (main) where

import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.List
import System.Process
import System.IO (hPutStrLn, stderr)

echoHeader :: IO ()
echoHeader = 
    readFile "license-generator/header.txt" >>= putStr

depsNames :: IO [String]
depsNames = do
    i <- readProcess "cabal-dependency-licenses" [] ""
    return $ sort $ map (drop 2) $ filter startsWithDash $ lines i
  where
    startsWithDash ('-' : _) = True
    startsWithDash _         = False

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
