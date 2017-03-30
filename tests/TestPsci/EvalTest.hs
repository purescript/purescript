module TestPsci.EvalTest where

import Prelude ()
import Prelude.Compat

import           Control.Monad (forM_, foldM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char (isSpace)
import           Data.List (dropWhileEnd, isPrefixOf, intercalate)
import           Data.List.Split (splitOn)
import           System.Directory (getCurrentDirectory)
import           System.Exit (exitFailure)
import           System.FilePath ((</>), takeFileName)
import qualified System.FilePath.Glob as Glob
import           System.IO.UTF8 (readUTF8File)
import           Test.HUnit
import           TestPsci.TestEnv

evalTests :: Test
evalTests = TestLabel "evalTests" . TestCase $ do
  putStrLn "\nPSCi eval test suites"
  testFiles <- evalTestFiles
  forM_ testFiles evalTest

evalTestFiles :: IO [FilePath]
evalTestFiles = do
  cwd <- getCurrentDirectory
  let psciExamples = cwd </> "examples" </> "psci"
  Glob.globDir1 (Glob.compile "**/*.purs") psciExamples

evalTest :: FilePath -> IO ()
evalTest f = do
  putStrLn $ "  " ++ takeFileName f
  ls <- trimAndFilter . lines <$> readUTF8File f
  execTestPSCi $ foldM_ go Nothing ls
  where
  go :: Maybe String -> String -> TestPSCi (Maybe String)
  go (Just expected) line = line `evaluatesTo` expected >> pure Nothing
  go Nothing         line =
    if "-- @" `isPrefixOf` line
    then
      case trimAndFilter $ splitOn " " $ drop 4 line of
        "shouldEvaluateTo" : results ->
          pure . Just $ intercalate " " results
        _ -> liftIO $ do
          putStrLn $ "invalid comment: " ++ line
          exitFailure
    else
      run line >> pure Nothing

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

trimAndFilter :: [String] -> [String]
trimAndFilter = filter (not . null) . map trim
