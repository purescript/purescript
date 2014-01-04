-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main where

import qualified Language.PureScript as P
import System.Console.CmdTheLine
import Control.Applicative
import Control.Monad (forM)
import System.Exit (exitSuccess, exitFailure)
import qualified System.IO.UTF8 as U
import Text.Parsec (ParseError)

readInput :: Maybe [FilePath] -> IO (Either ParseError [P.Module])
readInput Nothing = getContents >>= return . P.runIndentParser P.parseModules
readInput (Just input) = fmap (fmap concat . sequence) $ forM input $ \inputFile -> do
  text <- U.readFile inputFile
  return $ P.runIndentParser P.parseModules text

compile :: Maybe [FilePath] -> Maybe FilePath -> Maybe FilePath -> IO ()
compile input output externs = do
  modules <- readInput input
  case modules of
    Left err -> do
      U.print err
      exitFailure
    Right ms ->
      case P.compile ms of
        Left err -> do
          U.putStrLn err
          exitFailure
        Right (js, exts, _) -> do
          case output of
            Just path -> U.writeFile path js
            Nothing -> U.putStrLn js
          case externs of
            Nothing -> return ()
            Just filePath -> U.writeFile filePath exts
          exitSuccess

useStdIn :: Term Bool
useStdIn = value . flag $ (optInfo [ "s", "stdin" ])
     { optDoc = "Read from standard input" }

inputFiles :: Term [FilePath]
inputFiles = value $ posAny [] $ posInfo
     { posDoc = "The input .ps files" }

outputFile :: Term (Maybe FilePath)
outputFile = value $ opt Nothing $ (optInfo [ "o", "output" ])
     { optDoc = "The output .js file" }

externsFile :: Term (Maybe FilePath)
externsFile = value $ opt Nothing $ (optInfo [ "e", "externs" ])
     { optDoc = "The output .e.ps file" }

stdInOrInputFiles :: Term (Maybe [FilePath])
stdInOrInputFiles = combine <$> useStdIn <*> inputFiles
  where
  combine False input = Just input
  combine True _ = Nothing

term :: Term (IO ())
term = compile <$> stdInOrInputFiles <*> outputFile <*> externsFile

termInfo :: TermInfo
termInfo = defTI
  { termName = "psc"
  , version  = "1.0"
  , termDoc  = "Compiles PureScript to Javascript"
  }

main :: IO ()
main = run (term, termInfo)
