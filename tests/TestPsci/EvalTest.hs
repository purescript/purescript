module TestPsci.EvalTest where

import Prelude ()
import Prelude.Compat

import           Control.Monad (forM_, foldM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (isPrefixOf, intercalate)
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

data EvalLine = Line String
              | Comment EvalContext
              | Empty
              | Invalid String
              deriving (Show)

data EvalContext = ShouldEvaluateTo String
                 | None
                 deriving (Show)

evalCommentPrefix :: String
evalCommentPrefix = "-- @"

parseEvalLine :: String -> EvalLine
parseEvalLine "" = Empty
parseEvalLine line
  | evalCommentPrefix `isPrefixOf` line =
    case splitOn " " $ drop (length evalCommentPrefix) line of
      "shouldEvaluateTo" : args -> Comment (ShouldEvaluateTo $ intercalate " " args)
      _ -> Invalid line
  | otherwise = Line line

evalTest :: FilePath -> IO ()
evalTest f = do
  putStrLn $ "  " ++ takeFileName f
  evalLines <- map parseEvalLine . lines <$> readUTF8File f
  execTestPSCi $ foldM_ handleLine None evalLines

handleLine :: EvalContext -> EvalLine -> TestPSCi EvalContext
handleLine context Empty = pure context
handleLine None (Line stmt) = run stmt >> pure None
handleLine None (Comment context) = pure context
handleLine (ShouldEvaluateTo expected) (Line expr) = expr `evaluatesTo` expected >> pure None
handleLine _ line = liftIO $ putStrLn ("unexpected: " ++ show line) >> exitFailure
