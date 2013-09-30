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

import PureScript
import Text.Parsec
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import System.Console.CmdTheLine
import Control.Applicative
import System.Exit (exitSuccess, exitFailure)
import qualified System.IO.UTF8 as U

compile :: [FilePath] -> Maybe FilePath -> IO ()
compile inputFiles outputFile = do
  input <- fmap concat $ mapM U.readFile inputFiles
  let ast = parse parseDeclarations "" input
  case ast of
    Left err -> do
      U.print err
      exitFailure
    Right decls -> do
      case check (typeCheckAll decls) of
        Left typeError -> do
          U.putStrLn typeError
          exitFailure
        Right _ -> do
          let js = intercalate "\n" $ mapMaybe declToJs decls
          case outputFile of
            Just path -> U.writeFile path js
            Nothing -> U.putStrLn js
          exitSuccess

inputFiles :: Term [FilePath]
inputFiles = nonEmpty $ posAny [] $ posInfo
     { posDoc = "The input .ps files" }

outputFile :: Term (Maybe FilePath)
outputFile = value $ opt Nothing $ (optInfo [ "o", "output" ])
     { optDoc = "The output .js file" }

term :: Term (IO ())
term = compile <$> inputFiles <*> outputFile

termInfo :: TermInfo
termInfo = defTI
  { termName = "psc"
  , version  = "1.0"
  , termDoc  = "Compiles PureScript to Javascript"
  }

main = run (term, termInfo)
