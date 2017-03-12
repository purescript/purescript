module TestPsci.TestEnv where

import Prelude ()
import Prelude.Compat

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.RWS.Strict (evalRWST, RWST)
import qualified Language.PureScript as P
import           Language.PureScript.Interactive
import           System.Directory (getCurrentDirectory)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import qualified System.FilePath.Glob as Glob
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

testEval :: String -> TestPSCi ()
testEval = const $ return () -- not yet actually eval expr command

testReload :: TestPSCi ()
testReload = return ()

run :: String -> TestPSCi ()
run s = case parseCommand s of
          Left errStr -> liftIO $ putStrLn errStr >> exitFailure
          Right command ->
            handleCommand testEval testReload command

(@?==) :: (Eq a, Show a) => a -> a -> TestPSCi ()
x @?== y = liftIO $ x @?= y
