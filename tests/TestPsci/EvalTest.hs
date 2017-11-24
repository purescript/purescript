module TestPsci.EvalTest where

import PSPrelude

import qualified Data.Text as T
import           System.Directory (getCurrentDirectory)
import           System.FilePath ((</>), takeFileName)
import qualified System.FilePath.Glob as Glob
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

data EvalLine = Line Text
              | Comment EvalContext
              | Empty
              | Invalid Text
              deriving (Show)

data EvalContext = ShouldEvaluateTo Text
                 | Paste [Text]
                 | None
                 deriving (Show)

evalCommentPrefix :: Text
evalCommentPrefix = "-- @"

parseEvalLine :: Text -> EvalLine
parseEvalLine "" = Empty
parseEvalLine line =
  case T.stripPrefix evalCommentPrefix line of
    Just rest ->
      case T.splitOn " " rest of
        "shouldEvaluateTo" : args -> Comment (ShouldEvaluateTo $ T.intercalate " " args)
        "paste" : [] -> Comment (Paste [])
        _ -> Invalid line
    Nothing -> Line line

evalTest :: FilePath -> Spec
evalTest f = specify (takeFileName f) $ do
  evalLines <- map parseEvalLine . T.lines <$> readFile f
  execTestPSCi $ foldM_ handleLine None evalLines

handleLine :: EvalContext -> EvalLine -> TestPSCi EvalContext
handleLine ctx Empty = pure ctx
handleLine None (Line stmt) = run stmt >> pure None
handleLine None (Comment ctx) = pure ctx
handleLine (ShouldEvaluateTo expected) (Line expr) = expr `evaluatesTo` expected >> pure None
handleLine (Paste ls) (Line l) = pure . Paste $ ls <> [l]
handleLine (Paste ls) (Comment (Paste _)) = run (T.intercalate "\n" ls) >> pure None
handleLine _ line = liftIO $ putText ("unexpected: " <> show line) >> exitFailure
