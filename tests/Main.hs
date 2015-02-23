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

{-# LANGUAGE DataKinds, DoAndIfThenElse #-}

module Main (main) where

import qualified Language.PureScript as P

import Data.List (isSuffixOf)
import Data.Traversable (traverse)
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Reader (runReaderT)
import Control.Applicative
import System.Exit
import System.Process
import System.FilePath (pathSeparator)
import System.Directory (getCurrentDirectory, getTemporaryDirectory, getDirectoryContents, findExecutable)
import Text.Parsec (ParseError)

readInput :: [FilePath] -> IO [(FilePath, String)]
readInput inputFiles = forM inputFiles $ \inputFile -> do
  text <- readFile inputFile
  return (inputFile, text)

loadPrelude :: Either String (String, String, P.Environment)
loadPrelude =
  case P.parseModulesFromFiles id [("", P.prelude)] of
    Left parseError -> Left (show parseError)
    Right ms -> runReaderT (P.compile (map snd ms) []) $ P.defaultCompileOptions { P.optionsAdditional = P.CompileOptions "Tests" [] [] }

compile :: P.Options P.Compile -> [FilePath] -> IO (Either String (String, String, P.Environment))
compile opts inputFiles = do
  modules <- P.parseModulesFromFiles id <$> readInput inputFiles
  case modules of
    Left parseError ->
      return (Left $ show parseError)
    Right ms -> return $ runReaderT (P.compile (map snd ms) []) opts

assert :: FilePath -> P.Options P.Compile -> FilePath -> (Either String (String, String, P.Environment) -> IO (Maybe String)) -> IO ()
assert preludeExterns opts inputFile f = do
  e <- compile opts [preludeExterns, inputFile]
  maybeErr <- f e
  case maybeErr of
    Just err -> putStrLn err >> exitFailure
    Nothing -> return ()

assertCompiles :: String -> FilePath -> FilePath -> IO ()
assertCompiles preludeJs preludeExterns inputFile = do
  putStrLn $ "Assert " ++ inputFile ++ " compiles successfully"
  let options = P.defaultCompileOptions
                              { P.optionsMain = Just "Main"
                              , P.optionsAdditional = P.CompileOptions "Tests" ["Main"] ["Main"]
                              }
  assert preludeExterns options inputFile $ either (return . Just) $ \(js, _, _) -> do
    process <- findNodeProcess
    result <- traverse (\node -> readProcessWithExitCode node [] (preludeJs ++ js)) process
    case result of
      Just (ExitSuccess, out, _) -> putStrLn out >> return Nothing
      Just (ExitFailure _, _, err) -> return $ Just err
      Nothing -> return $ Just "Couldn't find node.js executable"

assertDoesNotCompile :: FilePath -> FilePath -> IO ()
assertDoesNotCompile preludeExterns inputFile = do
  putStrLn $ "Assert " ++ inputFile ++ " does not compile"
  assert preludeExterns (P.defaultCompileOptions { P.optionsAdditional = P.CompileOptions "Tests" [] [] }) inputFile $ \e ->
    case e of
      Left err -> putStrLn err >> return Nothing
      Right _ -> return $ Just "Should not have compiled"

findNodeProcess :: IO (Maybe String)
findNodeProcess = runMaybeT . msum $ map (MaybeT . findExecutable) names
    where names = ["nodejs", "node"]

main :: IO ()
main = do
  putStrLn "Compiling Prelude"
  case loadPrelude of
    Left err -> putStrLn err >> exitFailure
    Right (preludeJs, exts, _) -> do
      tmp <- getTemporaryDirectory
      let preludeExterns = tmp ++ pathSeparator : "prelude.externs"
      writeFile preludeExterns exts
      putStrLn $ "Wrote " ++ preludeExterns
      cd <- getCurrentDirectory
      let examples = cd ++ pathSeparator : "examples"
      let passing = examples ++ pathSeparator : "passing"
      passingTestCases <- getDirectoryContents passing
      forM_ passingTestCases $ \inputFile -> when (".purs" `isSuffixOf` inputFile) $
        assertCompiles preludeJs preludeExterns (passing ++ pathSeparator : inputFile)
      let failing = examples ++ pathSeparator : "failing"
      failingTestCases <- getDirectoryContents failing
      forM_ failingTestCases $ \inputFile -> when (".purs" `isSuffixOf` inputFile) $
        assertDoesNotCompile preludeExterns (failing ++ pathSeparator : inputFile)
      exitSuccess

