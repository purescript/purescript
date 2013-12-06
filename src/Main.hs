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

compile :: [FilePath] -> Maybe FilePath -> Maybe FilePath -> IO ()
compile input output externs = do
  asts <- fmap (fmap concat . sequence) $ forM input $ \inputFile -> do
    text <- U.readFile inputFile
    return $ P.runIndentParser P.parseDeclarations text
  case asts of
    Left err -> do
      U.print err
      exitFailure
    Right decls ->
      case P.compile decls of
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

inputFiles :: Term [FilePath]
inputFiles = nonEmpty $ posAny [] $ posInfo
     { posDoc = "The input .ps files" }

outputFile :: Term (Maybe FilePath)
outputFile = value $ opt Nothing $ (optInfo [ "o", "output" ])
     { optDoc = "The output .js file" }

externsFile :: Term (Maybe FilePath)
externsFile = value $ opt Nothing $ (optInfo [ "e", "externs" ])
     { optDoc = "The output .e.ps file" }

term :: Term (IO ())
term = compile <$> inputFiles <*> outputFile <*> externsFile

termInfo :: TermInfo
termInfo = defTI
  { termName = "psc"
  , version  = "1.0"
  , termDoc  = "Compiles PureScript to Javascript"
  }

main :: IO ()
main = run (term, termInfo)
