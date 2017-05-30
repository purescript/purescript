{-# LANGUAGE OverloadedStrings #-}
module TestPsci.CompletionTest where

import Prelude ()
import Prelude.Compat

import Test.Hspec

import           Control.Monad (mapM_)
import           Control.Monad.Trans.State.Strict (evalStateT)
import           Data.List (sort)
import qualified Data.Text as T
import qualified Language.PureScript as P
import           Language.PureScript.Interactive
import           System.Console.Haskeline
import           TestPsci.TestEnv (initTestPSCiEnv)
import           TestUtils (getSupportModuleNames)

completionTests :: Spec
completionTests = context "completionTests" $ do
  mns <- runIO $ getSupportModuleNames
  mapM_ assertCompletedOk (completionTestData mns)

-- If the cursor is at the right end of the line, with the 1st element of the
-- pair as the text in the line, then pressing tab should offer all the
-- elements of the list (which is the 2nd element) as completions.
completionTestData :: [T.Text] -> [(String, [String])]
completionTestData supportModuleNames =
  -- basic directives
  [ (":h",  [":help"])
  , (":r",  [":reload"])
  , (":c",  [":clear"])
  , (":q",  [":quit"])
  , (":b",  [":browse"])

  -- :browse should complete module names
  , (":b Control.Monad.E",    map (":b Control.Monad.Eff" ++) ["", ".Unsafe", ".Class", ".Console", ".Uncurried"])
  , (":b Control.Monad.Eff.", map (":b Control.Monad.Eff" ++) [".Unsafe", ".Class", ".Console", ".Uncurried"])

  -- import should complete module names
  , ("import Control.Monad.E",    map ("import Control.Monad.Eff" ++) ["", ".Unsafe", ".Class", ".Console", ".Uncurried"])
  , ("import Control.Monad.Eff.", map ("import Control.Monad.Eff" ++) [".Unsafe", ".Class", ".Console", ".Uncurried"])

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
  , ("import ", map (T.unpack . mappend "import ") supportModuleNames)
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

assertCompletedOk :: (String, [String]) -> Spec
assertCompletedOk (line, expecteds) = specify line $ do
  (unusedR, completions) <- runCM (completion' (reverse line, ""))
  let unused = reverse unusedR
  let actuals = map ((unused ++) . replacement) completions
  sort expecteds `shouldBe` sort actuals

runCM :: CompletionM a -> IO a
runCM act = do
  psciState <- getPSCiStateForCompletion
  evalStateT (liftCompletionM act) psciState

getPSCiStateForCompletion :: IO PSCiState
getPSCiStateForCompletion = do
  (PSCiState _ bs es, _) <- initTestPSCiEnv
  let imports = [controlMonadSTasST, (P.ModuleName [P.ProperName "Prelude"], P.Implicit, Nothing)]
  return $ PSCiState imports bs es

controlMonadSTasST :: ImportedModule
controlMonadSTasST = (s "Control.Monad.ST", P.Implicit, Just (s "ST"))
  where
  s = P.moduleNameFromString
