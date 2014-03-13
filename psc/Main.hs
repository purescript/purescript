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

import Control.Applicative
import Control.Monad (forM)

import Data.Version (showVersion)

import System.Console.CmdTheLine
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Exit (exitSuccess, exitFailure)

import Text.Parsec (ParseError)

import qualified Language.PureScript as P
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U

preludeFilename :: IO FilePath
preludeFilename = Paths.getDataFileName "prelude/prelude.purs"

readInput :: Maybe [FilePath] -> IO (Either ParseError [P.Module])
readInput Nothing = P.runIndentParser "" P.parseModules <$> getContents
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
            Just path -> mkdirp path >> U.writeFile path js
            Nothing -> U.putStrLn js
          case externs of
            Just path -> mkdirp path >> U.writeFile path exts
            Nothing -> return ()
          exitSuccess
  where
  mkdirp :: FilePath -> IO ()
  mkdirp = createDirectoryIfMissing True . takeDirectory

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

runMain :: Term (Maybe String)
runMain = value $ defaultOpt (Just "Main") Nothing $ (optInfo [ "main" ])
     { optDoc = "Generate code to run the main method in the specified module." }

noOpts :: Term Bool
noOpts = value $ flag $ (optInfo [ "no-opts" ])
     { optDoc = "Skip the optimization phase." }

browserNamespace :: Term String
browserNamespace = value $ opt "PS" $ (optInfo [ "browser-namespace" ])
     { optDoc = "Specify the namespace that PureScript modules will be exported to when running in the browser." }

dceModules :: Term [String]
dceModules = value $ optAll [] $ (optInfo [ "m", "module" ])
     { optDoc = "Enables dead code elimination, all code which is not a transitive dependency of a specified module will be removed. This argument can be used multiple times." }

codeGenModules :: Term [String]
codeGenModules = value $ optAll [] $ (optInfo [ "codegen" ])
     { optDoc = "A list of modules for which Javascript and externs should be generated. This argument can be used multiple times." }

options :: Term P.Options
options = P.Options <$> tco <*> performRuntimeTypeChecks <*> magicDo <*> runMain <*> noOpts <*> browserNamespace <*> dceModules <*> codeGenModules

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
  , version  = showVersion Paths.version
  , termDoc  = "Compiles PureScript to Javascript"
  }

main :: IO ()
main = do
  prelude <- preludeFilename
  run (term prelude, termInfo)
