module TestPsci.EvalTest where

import Prelude ()
import Prelude.Compat

import           Control.Monad (forM_, foldM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (stripPrefix, intercalate)
import           Data.List.Split (splitOn)
import           System.Directory (getCurrentDirectory)
import           System.Exit (exitFailure)
import           System.FilePath ((</>), takeFileName)
import qualified System.FilePath.Glob as Glob
import           System.IO.UTF8 (readUTF8File)
import           Test.Hspec
import           TestPsci.TestEnv

evalTests :: Spec
evalTests = context "evalTests" $ do
  testFiles <- runIO evalTestFiles
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
                 | Paste [String]
                 | None
                 deriving (Show)

evalCommentPrefix :: String
evalCommentPrefix = "-- @"

parseEvalLine :: String -> EvalLine
parseEvalLine "" = Empty
parseEvalLine line =
  case stripPrefix evalCommentPrefix line of
    Just rest ->
      case splitOn " " rest of
        "shouldEvaluateTo" : args -> Comment (ShouldEvaluateTo $ intercalate " " args)
        "paste" : [] -> Comment (Paste [])
        _ -> Invalid line
    Nothing -> Line line

evalTest :: FilePath -> Spec
evalTest f = specify (takeFileName f) $ do
  evalLines <- map parseEvalLine . lines <$> readUTF8File f
  execTestPSCi $ foldM_ handleLine None evalLines

handleLine :: EvalContext -> EvalLine -> TestPSCi EvalContext
handleLine ctx Empty = pure ctx
handleLine None (Line stmt) = run stmt >> pure None
handleLine None (Comment ctx) = pure ctx
handleLine (ShouldEvaluateTo expected) (Line expr) = expr `evaluatesTo` expected >> pure None
handleLine (Paste ls) (Line l) = pure . Paste $ ls ++ [l]
handleLine (Paste ls) (Comment (Paste _)) = run (intercalate "\n" ls) >> pure None
handleLine _ line = liftIO $ putStrLn ("unexpected: " ++ show line) >> exitFailure
