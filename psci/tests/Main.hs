{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Monad.Trans.State.Strict (runStateT)
import Control.Monad (when, forM)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.Writer.Strict (runWriterT)
import Control.Monad.Trans.Except (runExceptT)

import Data.List (sort)

import System.Exit (exitFailure)
import System.Console.Haskeline
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import qualified System.FilePath.Glob as Glob

import Test.HUnit

import qualified Language.PureScript as P

import PSCi
import Completion
import Types

import TestsSetup

main :: IO ()
main = do
  fetchSupportCode
  Counts{..} <- runTestTT allTests
  when (errors + failures > 0) exitFailure

allTests :: Test
allTests = completionTests

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
  [ (":h", [":help"])
  , (":re", [":reset"])
  , (":q", [":quit"])
  , (":mo", [":module"])
  , (":b", [":browse"])

  -- :browse should complete module names
  , (":b Control.Monad.E",    map (":b Control.Monad.Eff" ++) ["", ".Unsafe", ".Class", ".Console"])
  , (":b Control.Monad.Eff.", map (":b Control.Monad.Eff" ++) [".Unsafe", ".Class", ".Console"])

  -- import should complete module names
  , ("import Control.Monad.E",    map ("import Control.Monad.Eff" ++) ["", ".Unsafe", ".Class", ".Console"])
  , ("import Control.Monad.Eff.", map ("import Control.Monad.Eff" ++) [".Unsafe", ".Class", ".Console"])
  , ("import qualified Control.Monad.Eff.", map ("import qualified Control.Monad.Eff" ++) [".Unsafe", ".Class", ".Console"])

  -- :load, :module should complete file paths
  , (":l psci/tests/data/", [":l psci/tests/data/Sample.purs"])
  , (":module psci/tests/data/", [":module psci/tests/data/Sample.purs"])

  -- :quit, :help, :reset should not complete
  , (":help ", [])
  , (":quit ", [])
  , (":reset ", [])

  -- :show should complete to "loaded" and "import"
  , (":show ", [":show import", ":show loaded"])
  , (":show a", [])

  -- :type should complete values and data constructors in scope
  , (":type Control.Monad.Eff.Console.lo", [":type Control.Monad.Eff.Console.log"])
  , (":type uni", [":type unit"])
  , (":type E", [":type EQ"])

  -- :kind should complete types in scope
  , (":kind C", [":kind Control.Monad.Eff.Pure"])
  , (":kind O", [":kind Ordering"])

  -- Only one argument for directives should be completed
  , (":show import ", [])
  , (":type EQ ", [])
  , (":kind Ordering ", [])

  -- a few other import tests
  , ("impor", ["import"])
  , ("import q", ["import qualified"])
  , ("import ", map ("import " ++) supportModules ++ ["import qualified"])
  , ("import Prelude ", [])

  -- String and number literals should not be completed
  , ("\"hi", [])
  , ("34", [])

  -- Identifiers and data constructors should be completed
  , ("uni", ["unit"])
  , ("Control.Monad.Eff.Class.", ["Control.Monad.Eff.Class.liftEff"])
  , ("G", ["GT"])
  , ("Prelude.L", ["Prelude.LT"])

  -- if a module is imported qualified, values should complete under the
  -- qualified name, as well as the original name.
  , ("ST.new", ["ST.newSTRef"])
  , ("Control.Monad.ST.new", ["Control.Monad.ST.newSTRef"])
  ]
  where

assertCompletedOk :: (String, [String]) -> Assertion
assertCompletedOk (line, expecteds) = do
  (unusedR, completions) <- runCM (completion' (reverse line, ""))
  let unused = reverse unusedR
  let actuals = map ((unused ++) . replacement) completions
  sort expecteds @=? sort actuals

runCM :: CompletionM a -> IO a
runCM act = do
  psciState <- getPSCiState
  fmap fst (runStateT (liftCompletionM act) psciState)

getPSCiState :: IO PSCiState
getPSCiState = do
  cwd <- getCurrentDirectory
  let supportDir = cwd </> "tests" </> "support" </> "flattened"
  let supportFiles ext = Glob.globDir1 (Glob.compile ("*." ++ ext)) supportDir
  pursFiles <- supportFiles "purs"
  jsFiles   <- supportFiles "js"

  modulesOrFirstError <- loadAllModules pursFiles
  foreignFiles <- forM jsFiles (\f -> (f,) <$> readFile f)
  Right (foreigns, _) <- runExceptT $ runWriterT $ P.parseForeignModulesFromFiles foreignFiles
  case modulesOrFirstError of
    Left err ->
      print err >> exitFailure
    Right modules ->
      let imports = [controlMonadSTasST, (P.ModuleName [P.ProperName "Prelude"], P.Implicit, Nothing)]
      in  return (PSCiState [] imports modules foreigns [] [])

controlMonadSTasST :: ImportedModule
controlMonadSTasST = (s "Control.Monad.ST", P.Implicit, Just (s "ST"))
  where
  s = P.moduleNameFromString

supportModules :: [String]
supportModules =
  [ "Control.Monad.Eff.Class"
  , "Control.Monad.Eff.Console"
  , "Control.Monad.Eff"
  , "Control.Monad.Eff.Unsafe"
  , "Control.Monad.ST"
  , "Data.Function"
  , "Prelude"
  , "Test.Assert"
  ]
