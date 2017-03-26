module TestPsci.EvalTest where

import Prelude ()
import Prelude.Compat

import           Control.Monad (forM_)
import           Data.Char (isSpace)
import           Data.List.Split (splitOn)
import           System.Directory (getCurrentDirectory)
import           System.IO.UTF8 (readUTF8File)
import           System.FilePath ((</>), takeFileName)
import qualified System.FilePath.Glob as Glob
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
  execTestPSCi $ forM_ ls $ \ line ->
    case trimAndFilter $ splitOn " -- " line of
      [expr, expected] -> expr `evaluatesTo` expected
      _ -> run line

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

trimAndFilter :: [String] -> [String]
trimAndFilter = filter (not . null) . map trim
