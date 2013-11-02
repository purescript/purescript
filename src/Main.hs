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

import Language.PureScript
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import System.Console.CmdTheLine
import Control.Applicative
import Control.Monad (forM)
import System.Exit (exitSuccess, exitFailure)
import qualified Text.Parsec as P
import qualified System.IO.UTF8 as U
import qualified Data.Map as M

compile :: [FilePath] -> Maybe FilePath -> Maybe FilePath -> IO ()
compile inputFiles outputFile externsFile = do
  asts <- fmap (fmap concat . sequence) $ forM inputFiles $ \inputFile -> do
    text <- U.readFile inputFile
    return $ runIndentParser parseDeclarations text
  case asts of
    Left err -> do
      U.print err
      exitFailure
    Right decls ->
      case check (typeCheckAll decls) of
        Left typeError -> do
          U.putStrLn typeError
          exitFailure
        Right (_, env) -> do
          let js = intercalate "; " . map prettyPrintJS . concat . mapMaybe (declToJs) $ decls
          case outputFile of
            Just path -> U.writeFile path js
            Nothing -> U.putStrLn js
          case externsFile of
            Nothing -> return ()
            Just filePath -> U.writeFile filePath $ intercalate "\n" $ mapMaybe (externToPs env) decls
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

main = run (term, termInfo)
