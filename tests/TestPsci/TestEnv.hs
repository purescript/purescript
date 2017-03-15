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
import           Test.HUnit ((@?=))

type TestPSCi a = RWST PSCiConfig () PSCiState IO a

initTestPSCi :: IO (PSCiState, PSCiConfig)
initTestPSCi = do
  cwd <- getCurrentDirectory
  let supportDir = cwd </> "tests" </> "support" </> "bower_components"
  let supportFiles ext = Glob.globDir1 (Glob.compile ("purescript-*/src/**/*." ++ ext)) supportDir
  pursFiles <- supportFiles "purs"

  modulesOrFirstError <- loadAllModules pursFiles
  case modulesOrFirstError of
    Left err ->
      print err >> exitFailure
    Right modules -> do
      resultOrErrors <- runMake . make $ modules
      case resultOrErrors of
        Left errs -> putStrLn (P.prettyPrintMultipleErrors P.defaultPPEOptions errs) >> exitFailure
        Right (externs, env) ->
          return (PSCiState [] [] (zip (map snd modules) externs), PSCiConfig pursFiles env)

runTestPSCi :: TestPSCi a -> IO a
runTestPSCi i = do
  (s, c) <- initTestPSCi
  fst <$> evalRWST i c s

psciEval :: TestPSCi String
psciEval = liftIO $ do
  writeFile indexFile "require('$PSCI')['$main']();"
  process <- findNodeProcess
  result <- traverse (\node -> readProcessWithExitCode node [indexFile] "") process
  case result of
    Just (ExitSuccess, out, _)   -> return out
    Just (ExitFailure _, _, err) -> putStrLn err >> exitFailure
    Nothing                      -> putStrLn "Couldn't find node.js" >> exitFailure

runWithEval :: String -> TestPSCi () -> TestPSCi ()
runWithEval comm eval =
  case parseCommand comm of
    Left errStr -> liftIO $ putStrLn errStr >> exitFailure
    Right command -> handleCommand (\_ -> eval) (return ()) command

run :: String -> TestPSCi ()
run = flip runWithEval $ () <$ psciEval

equalsTo :: (Eq a, Show a) => a -> a -> TestPSCi ()
equalsTo x y = liftIO $ x @?= y

evaluatesTo :: String -> String -> TestPSCi ()
evaluatesTo command expected = runWithEval command $ do
  actual <- psciEval
  actual `equalsTo` (expected ++ "\n")
