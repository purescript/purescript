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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative
import Control.Monad.Error

import Data.Version (showVersion)

import System.Console.CmdTheLine
import System.Directory
       (doesFileExist, getModificationTime, createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Exit (exitSuccess, exitFailure)
import System.IO.Error (tryIOError)

import Text.Parsec (ParseError)

import qualified Language.PureScript as P
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U

preludeFilename :: IO FilePath
preludeFilename = Paths.getDataFileName "prelude/prelude.purs"

readInput :: [FilePath] -> IO (Either ParseError [(FilePath, P.Module)])
readInput input = fmap collect $ forM input $ \inputFile -> do
  text <- U.readFile inputFile
  return $ (inputFile, P.runIndentParser inputFile P.parseModules text)
  where
  collect :: [(FilePath, Either ParseError [P.Module])] -> Either ParseError [(FilePath, P.Module)]
  collect = fmap concat . sequence . map (\(fp, e) -> fmap (map ((,) fp)) e)

newtype Make a = Make { unMake :: ErrorT String IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadError String)

runMake :: Make a -> IO (Either String a)
runMake = runErrorT . unMake

makeIO :: IO a -> Make a
makeIO = Make . ErrorT . fmap (either (Left . show) Right) . tryIOError

instance P.MonadMake Make where
  getTimestamp path = makeIO $ do
    exists <- doesFileExist path
    case exists of
      True -> Just <$> getModificationTime path
      False -> return Nothing
  readTextFile path = makeIO $ do
    U.putStrLn $ "Reading " ++ path
    U.readFile path
  writeTextFile path text = makeIO $ do
    mkdirp path
    U.putStrLn $ "Writing " ++ path
    U.writeFile path text
  liftError = either throwError return
  progress = makeIO . U.putStrLn

compile :: FilePath -> P.Options -> [FilePath] -> IO ()
compile outputDir opts input = do
  modules <- readInput input
  case modules of
    Left err -> do
      U.print err
      exitFailure
    Right ms -> do
      e <- runMake $ P.make outputDir opts ms
      case e of
        Left err -> do
          U.putStrLn err
          exitFailure
        Right _ -> do
          exitSuccess

mkdirp :: FilePath -> IO ()
mkdirp = createDirectoryIfMissing True . takeDirectory

inputFiles :: Term [FilePath]
inputFiles = value $ posAny [] $ posInfo
     { posDoc = "The input .ps files" }

outputDirectory :: Term FilePath
outputDirectory = value $ opt "output" $ (optInfo [ "o", "output" ])
     { optDoc = "The output directory" }

noTco :: Term Bool
noTco = value $ flag $ (optInfo [ "no-tco" ])
     { optDoc = "Disable tail call optimizations" }

performRuntimeTypeChecks :: Term Bool
performRuntimeTypeChecks = value $ flag $ (optInfo [ "runtime-type-checks" ])
     { optDoc = "Generate runtime type checks" }

noPrelude :: Term Bool
noPrelude = value $ flag $ (optInfo [ "no-prelude" ])
     { optDoc = "Omit the Prelude" }

noMagicDo :: Term Bool
noMagicDo = value $ flag $ (optInfo [ "no-magic-do" ])
     { optDoc = "Disable the optimization that overloads the do keyword to generate efficient code specifically for the Eff monad." }

noOpts :: Term Bool
noOpts = value $ flag $ (optInfo [ "no-opts" ])
     { optDoc = "Skip the optimization phase." }

verboseErrors :: Term Bool
verboseErrors = value $ flag $ (optInfo [ "v", "verbose-errors" ])
     { optDoc = "Display verbose error messages" }

options :: Term P.Options
options = P.Options <$> noPrelude <*> noTco <*> performRuntimeTypeChecks <*> noMagicDo <*> pure Nothing <*> noOpts <*> pure Nothing <*> pure [] <*> pure [] <*> verboseErrors

inputFilesAndPrelude :: FilePath -> Term [FilePath]
inputFilesAndPrelude prelude = combine <$> (not <$> noPrelude) <*> inputFiles
  where
  combine True input = prelude : input
  combine False input = input

term :: FilePath -> Term (IO ())
term prelude = compile <$> outputDirectory <*> options <*> inputFilesAndPrelude prelude

termInfo :: TermInfo
termInfo = defTI
  { termName = "psc-make"
  , version  = showVersion Paths.version
  , termDoc  = "Compiles PureScript to Javascript"
  }

main :: IO ()
main = do
  prelude <- preludeFilename
  run (term prelude, termInfo)
