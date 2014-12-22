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

{-# LANGUAGE DataKinds, TupleSections, DoAndIfThenElse #-}

module Main (main) where

import qualified Language.PureScript as P

import Data.Maybe (catMaybes)
import Data.List (isSuffixOf)
import Data.Monoid ((<>))
import Data.Traversable (traverse)
import Control.Monad
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Applicative
import System.Exit
import System.Process
import System.FilePath (pathSeparator, (</>))
import System.Directory (getCurrentDirectory, getTemporaryDirectory, getDirectoryContents, findExecutable)
import Text.Parsec (ParseError)
import qualified System.IO.UTF8 as U

readInput :: [FilePath] -> IO [(FilePath, String)]
readInput inputFiles = forM inputFiles $ \inputFile -> do
  text <- U.readFile inputFile
  return (inputFile, text)

loadPrelude :: Either String (String, [String], P.Environment)
loadPrelude = 
  case P.parseModulesFromFiles id (map ("",) P.preludeModules) of
    Left parseError -> Left (show parseError)
    Right ms -> P.compile (P.defaultCompileOptions { P.optionsAdditional = P.CompileOptions "Tests" [] [] }) (map snd ms) []

compile :: P.Options P.Compile -> [FilePath] -> IO (Either String (String, [String], P.Environment))
compile opts inputFiles = do
  modules <- P.parseModulesFromFiles id <$> readInput inputFiles
  case modules of
    Left parseError ->
      return (Left $ show parseError)
    Right ms -> return $ P.compile opts (map snd ms) []

assert :: [FilePath] -> P.Options P.Compile -> FilePath -> (Either String (String, [String], P.Environment) -> IO (Maybe String)) -> IO (Maybe String)
assert preludeExterns opts inputFile f = do
  e <- compile opts (preludeExterns ++ [inputFile])
  maybeErr <- f e
  case maybeErr of
    Just err -> putStrLn err >> return (Just err)
    Nothing -> return Nothing

assertCompiles :: String -> [FilePath] -> FilePath -> IO (Maybe String)
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

assertDoesNotCompile :: [FilePath] -> FilePath -> IO (Maybe String)
assertDoesNotCompile preludeExterns inputFile = do
  putStrLn $ "Assert " ++ inputFile ++ " does not compile"
  assert preludeExterns (P.defaultCompileOptions { P.optionsAdditional = P.CompileOptions "Tests" [] [] }) inputFile $ \e ->
    case e of
      Left _ -> return Nothing
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
      preludeExterns <- forM (zip [1..] exts) $ \(i, content) -> do
        let filename = tmp </> show i <> ".externs"
        writeFile filename content
        putStrLn $ "Wrote " ++ filename
        return filename
      cd <- getCurrentDirectory
      let examples = cd ++ pathSeparator : "examples"
      let passing = examples ++ pathSeparator : "passing"
      passingTestCases <- getDirectoryContents passing
      results1 <- forM (filter (".purs" `isSuffixOf`) passingTestCases) $ \inputFile -> 
        assertCompiles preludeJs preludeExterns (passing ++ pathSeparator : inputFile)
      let failing = examples ++ pathSeparator : "failing"
      failingTestCases <- getDirectoryContents failing
      results2 <- forM (filter (".purs" `isSuffixOf`) failingTestCases) $ \inputFile -> 
        assertDoesNotCompile preludeExterns (failing ++ pathSeparator : inputFile)
      case catMaybes (results1 <> results2) of
        [] -> exitSuccess
        xs -> do
          putStrLn $ show (length xs) <> " test cases failed"
          exitFailure 

