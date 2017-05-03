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

-- | Run a PSCi command and evaluate the output with 'eval'.
runAndEval :: String -> TestPSCi () -> TestPSCi ()
runAndEval comm eval =
  case parseCommand comm of
    Left errStr -> liftIO $ putStrLn errStr >> exitFailure
    Right command ->
      -- the JS result can be ignored, as it's already written in a source file
      -- for the detail, please refer to Interactive.hs
      handleCommand (\_ -> eval) (return ()) (\_ -> return ()) command

-- | Run a PSCi command and ignore the output
run :: String -> TestPSCi ()
run comm = runAndEval comm $ jsEval *> return ()

-- | A lifted evaluation of Hspec 'shouldBe' for the TestPSCi
equalsTo :: (Eq a, Show a) => a -> a -> TestPSCi ()
equalsTo x y = liftIO $ x `shouldBe` y

-- | An assertion to check if a command evaluates to a string
evaluatesTo :: String -> String -> TestPSCi ()
evaluatesTo command expected = runAndEval command $ do
  actual <- jsEval
  actual `equalsTo` (expected ++ "\n")
