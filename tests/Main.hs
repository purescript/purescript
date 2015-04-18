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
import Control.Monad.Writer (runWriterT)
import Control.Applicative
import System.Exit
import System.Process
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory, getTemporaryDirectory, getDirectoryContents, findExecutable)
import Text.Parsec (ParseError)

readInput :: [FilePath] -> IO [(FilePath, String)]
readInput inputFiles = forM inputFiles $ \inputFile -> do
  text <- readFile inputFile
  return (inputFile, text)

loadPrelude :: [(FilePath, String)] -> Either P.MultipleErrors (String, String, P.Environment)
loadPrelude ms =
  case P.parseModulesFromFiles id ms of
    Left parseError -> Left . P.errorMessage . P.ErrorParsingPrelude $ parseError
    Right ms -> fmap fst . runWriterT $ runReaderT (P.compile (map snd ms) []) (P.defaultCompileOptions { P.optionsAdditional = P.CompileOptions "Tests" [] [] })

compile :: P.Options P.Compile -> [FilePath] -> IO (Either P.MultipleErrors (String, String, P.Environment))
compile opts inputFiles = do
  modules <- P.parseModulesFromFiles id <$> readInput inputFiles
  case modules of
    Left parseError ->
      return . Left . P.errorMessage . P.ErrorParsingPrelude $ parseError
    Right ms -> return $ fmap fst . runWriterT $ runReaderT (P.compile (map snd ms) []) opts

assert :: FilePath -> P.Options P.Compile -> FilePath -> (Either P.MultipleErrors (String, String, P.Environment) -> IO (Maybe String)) -> IO ()
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
  assert preludeExterns options inputFile $ either (return . Just . P.prettyPrintMultipleErrors False) $ \(js, _, _) -> do
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
      Left errs -> putStrLn  (P.prettyPrintMultipleErrors False errs) >> return Nothing
      Right _ -> return $ Just "Should not have compiled"

findNodeProcess :: IO (Maybe String)
findNodeProcess = runMaybeT . msum $ map (MaybeT . findExecutable) names
  where 
  names = ["nodejs", "node"]

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  prelude <- readInput [cwd </> "tests" </> "prelude.purs"]
  putStrLn "Compiling Prelude"
  case loadPrelude prelude of
    Left errs -> putStrLn (P.prettyPrintMultipleErrors False errs) >> exitFailure
    Right (preludeJs, exts, _) -> do
      tmp <- getTemporaryDirectory
      let preludeExterns = tmp </> "prelude.externs"
      writeFile preludeExterns exts
      putStrLn $ "Wrote " ++ preludeExterns
      let passing = cwd </> "examples" </> "passing"
      passingTestCases <- getDirectoryContents passing
      forM_ passingTestCases $ \inputFile -> when (".purs" `isSuffixOf` inputFile) $
        assertCompiles preludeJs preludeExterns (passing </> inputFile)
      let failing = cwd </> "examples" </> "failing"
      failingTestCases <- getDirectoryContents failing
      forM_ failingTestCases $ \inputFile -> when (".purs" `isSuffixOf` inputFile) $
        assertDoesNotCompile preludeExterns (failing </> inputFile)
      exitSuccess
