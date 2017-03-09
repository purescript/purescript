{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module TestPsci where

import Prelude ()
import Prelude.Compat

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (evalStateT)
import Control.Monad.Trans.RWS.Strict (evalRWST, RWST, get)
import Control.Monad (when)

import Data.List (sort)
import qualified Data.Text as T

import System.Exit (exitFailure)
import System.Console.Haskeline
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import qualified System.FilePath.Glob as Glob

import Test.HUnit

import qualified Language.PureScript as P

import Language.PureScript.Interactive

import TestUtils (supportModules)

main :: IO ()
main = do
  Counts{..} <- runTestTT allTests
  when (errors + failures > 0) exitFailure

allTests :: Test
allTests = TestList [ completionTests
                    , commandTests
                    ]

completionTests :: Test
completionTests =
  TestLabel "completionTests"
    (TestList (map (TestCase . assertCompletedOk) completionTestData))

-- If the cursor is at the right end of the line, with the 1st element of the
-- pair as the text in the line, then pressing tab should offer all the
-- elements of the list (which is the 2nd element) as completions.
completionTestData :: [(String, [String])]
completionTestData =
  -- basic directives
  [ (":h",  [":help"])
  , (":r",  [":reload"])
  , (":c",  [":clear"])
  , (":q",  [":quit"])
  , (":b",  [":browse"])

  -- :browse should complete module names
  , (":b Control.Monad.E",    map (":b Control.Monad.Eff" ++) ["", ".Unsafe", ".Class", ".Console"])
  , (":b Control.Monad.Eff.", map (":b Control.Monad.Eff" ++) [".Unsafe", ".Class", ".Console"])

  -- import should complete module names
  , ("import Control.Monad.E",    map ("import Control.Monad.Eff" ++) ["", ".Unsafe", ".Class", ".Console"])
  , ("import Control.Monad.Eff.", map ("import Control.Monad.Eff" ++) [".Unsafe", ".Class", ".Console"])

  -- :quit, :help, :reload, :clear should not complete
  , (":help ", [])
  , (":quit ", [])
  , (":reload ", [])
  , (":clear ", [])

  -- :show should complete to "loaded" and "import"
  , (":show ", [":show import", ":show loaded"])
  , (":show a", [])

  -- :type should complete values and data constructors in scope
  , (":type Control.Monad.Eff.Console.lo", [":type Control.Monad.Eff.Console.log", ":type Control.Monad.Eff.Console.logShow"])
  --, (":type uni", [":type unit"])
  --, (":type E", [":type EQ"])

  -- :kind should complete types in scope
  --, (":kind C", [":kind Control.Monad.Eff.Pure"])
  --, (":kind O", [":kind Ordering"])

  -- Only one argument for directives should be completed
  , (":show import ", [])
  , (":type EQ ", [])
  , (":kind Ordering ", [])

  -- a few other import tests
  , ("impor", ["import"])
  , ("import ", map ("import " ++) supportModules)
  , ("import Prelude ", [])

  -- String and number literals should not be completed
  , ("\"hi", [])
  , ("34", [])

  -- Identifiers and data constructors should be completed
  --, ("uni", ["unit"])
  , ("Control.Monad.Eff.Class.", ["Control.Monad.Eff.Class.liftEff"])
  --, ("G", ["GT"])
  , ("Data.Ordering.L", ["Data.Ordering.LT"])

  -- if a module is imported qualified, values should complete under the
  -- qualified name, as well as the original name.
  , ("ST.new", ["ST.newSTRef"])
  , ("Control.Monad.ST.new", ["Control.Monad.ST.newSTRef"])
  ]

assertCompletedOk :: (String, [String]) -> Assertion
assertCompletedOk (line, expecteds) = do
  (unusedR, completions) <- runCM (completion' (reverse line, ""))
  let unused = reverse unusedR
  let actuals = map ((unused ++) . replacement) completions
  sort expecteds @=? sort actuals

runCM :: CompletionM a -> IO a
runCM act = do
  psciState <- getPSCiStateForCompletion
  evalStateT (liftCompletionM act) psciState

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

getPSCiStateForCompletion :: IO PSCiState
getPSCiStateForCompletion = do
  (PSCiState _ bs es, _) <- initTestPSCi
  let imports = [controlMonadSTasST, (P.ModuleName [P.ProperName (T.pack "Prelude")], P.Implicit, Nothing)]
  return $ PSCiState imports bs es

controlMonadSTasST :: ImportedModule
controlMonadSTasST = (s "Control.Monad.ST", P.Implicit, Just (s "ST"))
  where
  s = P.moduleNameFromString . T.pack

type TestPSCi a = RWST PSCiConfig () PSCiState IO a

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

commandTests :: Test
commandTests = TestLabel "commandTests" $ TestList $ map (TestCase . runTestPSCi)
  [ do
      run "import Prelude"
      run "import Data.Functor"
      run "import Control.Monad"
      before <- psciImportedModules <$> get
      liftIO $ length before @?= 3
      run ":clear"
      after <- psciImportedModules <$> get
      liftIO $ length after @?= 0
  , do
      run "import Prelude"
      run "import Data.Functor"
      run "import Control.Monad"
      before <- psciImportedModules <$> get
      liftIO $ length before @?= 3
      run ":reload"
      after <- psciImportedModules <$> get
      liftIO $ length after @?= 3
  ]
