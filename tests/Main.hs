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

{-# LANGUAGE DataKinds, DoAndIfThenElse, TupleSections #-}

module Main (main) where

import qualified Language.PureScript as P
import qualified Language.PureScript.CodeGen.JS.Compile as JS
import Data.List (isSuffixOf)
import Data.Traversable (traverse)

import qualified Data.Map as M

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriterT)
import Control.Monad.Trans.Except (runExceptT)
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

loadPrelude :: [(FilePath, String)] -> M.Map P.ModuleName (FilePath, P.ForeignJS) -> Either P.MultipleErrors (String, String)
loadPrelude fs foreigns = do
  ms <- P.parseModulesFromFiles id fs
  fmap fst . runWriterT $ runReaderT (JS.compileJS (map snd ms) foreigns []) (P.defaultCompileOptions { P.optionsAdditional = P.CompileOptions "Tests" [] [] })

compile :: P.Options P.Compile -> [FilePath] -> M.Map P.ModuleName (FilePath, P.ForeignJS) -> IO (Either P.MultipleErrors (String, String))
compile opts inputFiles foreigns = runExceptT . fmap fst . runWriterT . flip runReaderT opts $ do
  fs <- liftIO $ readInput inputFiles
  ms <- P.parseModulesFromFiles id fs
  JS.compileJS (map snd ms) foreigns []

assert :: FilePath -> P.Options P.Compile -> FilePath -> M.Map P.ModuleName (FilePath, P.ForeignJS) -> (Either P.MultipleErrors (String, String) -> IO (Maybe String)) -> IO ()
assert preludeExterns opts inputFile foreigns f = do
  e <- compile opts [preludeExterns, inputFile] foreigns
  maybeErr <- f e
  case maybeErr of
    Just err -> putStrLn err >> exitFailure
    Nothing -> return ()

assertCompiles :: String -> FilePath -> FilePath -> M.Map P.ModuleName (FilePath, P.ForeignJS) -> IO ()
assertCompiles preludeJs preludeExterns inputFile foreigns = do
  putStrLn $ "Assert " ++ inputFile ++ " compiles successfully"
  let options = P.defaultCompileOptions
                              { P.optionsMain = Just "Main"
                              , P.optionsAdditional = P.CompileOptions "Tests" ["Main"] ["Main"]
                              }
  assert preludeExterns options inputFile foreigns $ either (return . Just . P.prettyPrintMultipleErrors False) $ \(js, _) -> do
    process <- findNodeProcess
    result <- traverse (\node -> readProcessWithExitCode node [] (preludeJs ++ js)) process
    case result of
      Just (ExitSuccess, out, _) -> putStrLn out >> return Nothing
      Just (ExitFailure _, _, err) -> return $ Just err
      Nothing -> return $ Just "Couldn't find node.js executable"

assertDoesNotCompile :: FilePath -> FilePath -> M.Map P.ModuleName (FilePath, P.ForeignJS) -> IO ()
assertDoesNotCompile preludeExterns inputFile foreigns = do
  putStrLn $ "Assert " ++ inputFile ++ " does not compile"
  assert preludeExterns (P.defaultCompileOptions { P.optionsAdditional = P.CompileOptions "Tests" [] [] }) inputFile foreigns $ \e ->
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
  let preludeDir = cwd </> "tests" </> "prelude"
      jsDir = preludeDir </> "js"
  prelude <- readInput [preludeDir </> "Prelude.purs"]
  jsFiles <- map (jsDir </>) . filter (".js" `isSuffixOf`) <$> getDirectoryContents jsDir
  foreignFiles <- forM jsFiles (\f -> (f,) <$> readFile f)
  Right (foreigns, _) <- runExceptT $ runWriterT $ P.parseForeignModulesFromFiles foreignFiles
  putStrLn "Compiling Prelude"
  case loadPrelude prelude foreigns of
    Left errs -> putStrLn (P.prettyPrintMultipleErrors False errs) >> exitFailure
    Right (preludeJs, exts) -> do
      tmp <- getTemporaryDirectory
      let preludeExterns = tmp </> "prelude.externs"
      writeFile preludeExterns exts
      putStrLn $ "Wrote " ++ preludeExterns
      let passing = cwd </> "examples" </> "passing"
      passingTestCases <- getDirectoryContents passing
      forM_ passingTestCases $ \inputFile -> when (".purs" `isSuffixOf` inputFile) $
        assertCompiles preludeJs preludeExterns (passing </> inputFile) foreigns
      let failing = cwd </> "examples" </> "failing"
      failingTestCases <- getDirectoryContents failing
      forM_ failingTestCases $ \inputFile -> when (".purs" `isSuffixOf` inputFile) $
        assertDoesNotCompile preludeExterns (failing </> inputFile) foreigns
      exitSuccess
