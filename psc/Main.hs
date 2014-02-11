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
import qualified Paths_purescript as Paths
import Data.Version (showVersion)

preludeFilename :: IO FilePath
preludeFilename = Paths.getDataFileName "prelude/prelude.purs"

readInput :: Maybe [FilePath] -> IO (Either ParseError [P.Module])
readInput Nothing = getContents >>= return . P.runIndentParser "" P.parseModules
readInput (Just input) = fmap (fmap concat . sequence) $ forM input $ \inputFile -> do
  text <- U.readFile inputFile
  return $ P.runIndentParser inputFile P.parseModules text

compile :: P.Options -> Maybe [FilePath] -> Maybe FilePath -> Maybe FilePath -> IO ()
compile opts input output externs = do
  modules <- readInput input
  case modules of
    Left err -> do
      U.print err
      exitFailure
    Right ms ->
      case P.compile opts ms of
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

tco :: Term Bool
tco = value $ flag $ (optInfo [ "tco" ])
     { optDoc = "Perform tail call optimizations" }

performRuntimeTypeChecks :: Term Bool
performRuntimeTypeChecks = value $ flag $ (optInfo [ "runtime-type-checks" ])
     { optDoc = "Generate runtime type checks" }

noPrelude :: Term Bool
noPrelude = value $ flag $ (optInfo [ "no-prelude" ])
     { optDoc = "Omit the Prelude" }

magicDo :: Term Bool
magicDo = value $ flag $ (optInfo [ "magic-do" ])
     { optDoc = "Overload the do keyword to generate efficient code specifically for the Eff monad." }

runMain :: Term Bool
runMain = value $ flag $ (optInfo [ "run-main" ])
     { optDoc = "Generate code to run the main method in the Main module." }

noOpts :: Term Bool
noOpts = value $ flag $ (optInfo [ "no-opts" ])
     { optDoc = "Skip the optimization phase." }

browserNamespace :: Term String
browserNamespace = value $ opt "PS" $ (optInfo [ "browser-namespace" ])
     { optDoc = "Specify the namespace that PureScript modules will be exported to when running in the browser." }

options :: Term P.Options
options = P.Options <$> tco <*> performRuntimeTypeChecks <*> magicDo <*> runMain <*> noOpts <*> browserNamespace

stdInOrInputFiles :: FilePath -> Term (Maybe [FilePath])
stdInOrInputFiles prelude = combine <$> useStdIn <*> (not <$> noPrelude) <*> inputFiles
  where
  combine False True input = Just (prelude : input)
  combine False False input = Just input
  combine True _ _ = Nothing

term :: FilePath -> Term (IO ())
term prelude = compile <$> options <*> stdInOrInputFiles prelude <*> outputFile <*> externsFile

termInfo :: TermInfo
termInfo = defTI
  { termName = "psc"
  , version  = showVersion $ Paths.version
  , termDoc  = "Compiles PureScript to Javascript"
  }

main :: IO ()
main = do
  prelude <- preludeFilename
  run (term prelude, termInfo)
