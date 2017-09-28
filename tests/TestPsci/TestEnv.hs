module TestPsci.TestEnv where

import Prelude ()
import Prelude.Compat

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.RWS.Strict (evalRWST, RWST)
import qualified Language.PureScript as P
import           Language.PureScript.Interactive
import           System.Directory (getCurrentDirectory)
import           System.Exit
import           System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob
import           System.Process (readProcessWithExitCode)
import           Test.Hspec (shouldBe)

-- | A monad transformer for handle PSCi actions in tests
type TestPSCi a = RWST PSCiConfig () PSCiState IO a

-- | Initialise PSCi state and config for tests
initTestPSCiEnv :: IO (PSCiState, PSCiConfig)
initTestPSCiEnv = do
  -- Load test support packages
  cwd <- getCurrentDirectory
  let supportDir = cwd </> "tests" </> "support" </> "bower_components"
  let supportFiles ext = Glob.globDir1 (Glob.compile ("purescript-*/src/**/*." ++ ext)) supportDir
  pursFiles <- supportFiles "purs"
  modulesOrError <- loadAllModules pursFiles
  case modulesOrError of
    Left err ->
      print err >> exitFailure
    Right modules -> do
      -- Make modules
      makeResultOrError <- runMake . make $ modules
      case makeResultOrError of
        Left errs -> putStrLn (P.prettyPrintMultipleErrors P.defaultPPEOptions errs) >> exitFailure
        Right (externs, env) ->
          return (PSCiState [] [] (zip (map snd modules) externs), PSCiConfig pursFiles env)

-- | Execute a TestPSCi, returning IO
execTestPSCi :: TestPSCi a -> IO a
execTestPSCi i = do
  (s, c) <- initTestPSCiEnv -- init state and config
  fst <$> evalRWST i c s

-- | Evaluate JS to which a PSCi input is compiled. The actual JS input is not
-- needed as an argument, as it is already written in the file during the
-- command evaluation.
jsEval :: TestPSCi String
jsEval = liftIO $ do
  writeFile indexFile "require('$PSCI')['$main']();"
  process <- findNodeProcess
  result <- traverse (\node -> readProcessWithExitCode node [indexFile] "") process
  case result of
    Just (ExitSuccess, out, _)   -> return out
    Just (ExitFailure _, _, err) -> putStrLn err >> exitFailure
    Nothing                      -> putStrLn "Couldn't find node.js" >> exitFailure

-- | Run a PSCi command and evaluate its outputs:
-- * jsOutputEval is used to evaluate compiled JS output by PSCi
-- * printedOutputEval is used to evaluate text printed directly by PSCi itself
runAndEval :: String -> TestPSCi () -> (String -> TestPSCi ()) -> TestPSCi ()
runAndEval comm jsOutputEval textOutputEval =
  case parseCommand comm of
    Left errStr -> liftIO $ putStrLn errStr >> exitFailure
    Right command ->
      -- The JS result is ignored, as it's already written in a JS source file.
      -- For the detail, please refer to Interactive.hs
      handleCommand (\_ -> jsOutputEval) (return ()) textOutputEval command

-- | Run a PSCi command, evaluate compiled JS, and ignore evaluation output and printed output
run :: String -> TestPSCi ()
run comm = runAndEval comm evalJsAndIgnore ignorePrinted
  where
    evalJsAndIgnore = jsEval *> return ()
    ignorePrinted _ = return ()

-- | A lifted evaluation of Hspec 'shouldBe' for the TestPSCi
equalsTo :: (Eq a, Show a) => a -> a -> TestPSCi ()
equalsTo x y = liftIO $ x `shouldBe` y

-- | An assertion to check command evaluated javascript output against a given string
evaluatesTo :: String -> String -> TestPSCi ()
evaluatesTo command expected = runAndEval command evalJsAndCompare ignorePrinted
  where
    evalJsAndCompare = do
      actual <- jsEval
      actual `equalsTo` (expected ++ "\n")
    ignorePrinted _ = return ()

-- | An assertion to check command PSCi printed output against a given string
prints :: String -> String -> TestPSCi ()
prints command expected = runAndEval command evalJsAndIgnore evalPrinted
  where
    evalJsAndIgnore = jsEval *> return ()
    evalPrinted s = s `equalsTo` expected
