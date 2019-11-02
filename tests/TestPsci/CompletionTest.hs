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
import           TestPsci.TestEnv (initTestPSCiEnv)
import           TestUtils (getSupportModuleNames)

completionTests :: Spec
completionTests = context "completionTests" $ do
  mns       <- runIO getSupportModuleNames
  psciState <- runIO getPSCiStateForCompletion
  mapM_ (assertCompletedOk psciState) (completionTestData mns)

-- If the cursor is at the right end of the line, with the 1st element of the
-- pair as the text in the line, then pressing tab should offer all the
-- elements of the list (which is the 2nd element) as completions.
completionTestData :: [T.Text] -> [(String, [String])]
completionTestData supportModuleNames =
  -- basic directives
  [ (":h",  [":help"])
  , (":r",  [":reload"])
  , (":c",  [":clear", ":complete"])
  , (":q",  [":quit"])
  , (":b",  [":browse"])

  -- :browse should complete module names
  , (":b Eff",    map (":b Effect" ++) ["", ".Unsafe", ".Class", ".Class.Console", ".Console", ".Uncurried", ".Ref"])
  , (":b Effect.", map (":b Effect" ++) [".Unsafe", ".Class", ".Class.Console", ".Console", ".Uncurried", ".Ref"])

  -- import should complete module names
  , ("import Eff",    map ("import Effect" ++) ["", ".Unsafe", ".Class", ".Class.Console", ".Console", ".Uncurried", ".Ref"])
  , ("import Effect.", map ("import Effect" ++) [".Unsafe", ".Class", ".Class.Console", ".Console", ".Uncurried", ".Ref"])

  -- :quit, :help, :reload, :clear should not complete
  , (":help ", [])
  , (":quit ", [])
  , (":reload ", [])
  , (":clear ", [])

  -- :show should complete its available arguments
  , (":show ", [":show import", ":show loaded", ":show print"])
  , (":show a", [])

  -- :type should complete next word from values and constructors in scope
  , (":type uni", [":type unit"])
  , (":type E", [":type EQ"])
  , (":type P.", map (":type P." ++) ["EQ", "GT", "LT", "unit"]) -- import Prelude (unit, Ordering(..)) as P
  , (":type Effect.Console.lo", [])
  , (":type voi", [])

  -- :kind should complete next word from types in scope
  , (":kind Str", [":kind String"])
  , (":kind ST.", [":kind ST.ST"]) -- import Control.Monad.ST as ST
  , (":kind STRef.", [":kind STRef.STRef"]) -- import Control.Monad.ST.Ref as STRef
  , (":kind Effect.", [])

  -- Only one argument for these directives should be completed
  , (":show import ", [])
  , (":browse Data.List ", [])

  -- These directives take any number of completable terms
  , (":type const compa", [":type const compare", ":type const comparing"])
  , (":kind Array In", [":kind Array Int"])

  -- a few other import tests
  , ("impor", ["import"])
  , ("import ", map (T.unpack . mappend "import ") supportModuleNames)
  , ("import Prelude ", [])

  -- String and number literals should not be completed
  , ("\"hi", [])
  , ("34", [])

  -- Identifiers and data constructors in scope should be completed
  , ("uni", ["unit"])
  , ("G", ["GT"])
  , ("P.G", ["P.GT"])
  , ("P.uni", ["P.unit"])
  , ("voi", []) -- import Prelude hiding (void)
  , ("Effect.Class.", [])

  -- complete first name after type annotation symbol
  , ("1 :: I", ["1 :: Int"])
  , ("1 ::I",  ["1 ::Int"])
  , ("1:: I",  ["1:: Int"])
  , ("1::I",   ["1::Int"])
  , ("(1::Int) uni", ["(1::Int) unit"]) -- back to completing values

  -- Parens and brackets aren't considered part of the current identifier
  , ("map id [uni", ["map id [unit"])
  , ("map (cons", ["map (const"])
  ]

assertCompletedOk :: PSCiState -> (String, [String]) -> Spec
assertCompletedOk psciState (line, expecteds) = specify line $ do
  results <- runCM psciState (completion' (reverse line, ""))
  let actuals = formatCompletions results
  sort actuals `shouldBe` sort expecteds

runCM :: PSCiState -> CompletionM a -> IO a
runCM psciState act = evalStateT (liftCompletionM act) psciState

getPSCiStateForCompletion :: IO PSCiState
getPSCiStateForCompletion = do
  (st, _) <- initTestPSCiEnv
  let imports = [-- import Control.Monad.ST as S
                 (qualName "Control.Monad.ST"
                    ,P.Implicit
                    ,Just (qualName "ST"))
                , -- import Control.Monad.ST.Ref as STRef
                 (qualName "Control.Monad.ST.Ref"
                    ,P.Implicit
                    ,Just (qualName "STRef"))
                 -- import Prelude hiding (void)
                ,(qualName "Prelude"
                    ,P.Hiding [valName "void"]
                    ,Nothing)
                 -- import Prelude (unit, Ordering(..)) as P
                ,(qualName "Prelude"
                    ,P.Explicit [valName "unit", typeName "Ordering"]
                    ,Just (qualName "P"))]
  return $ updateImportedModules (const imports) st
  where
    qualName   = P.moduleNameFromString
    valName    = P.ValueRef srcSpan . P.Ident
    typeName t = P.TypeRef srcSpan (P.ProperName t) Nothing
    srcSpan    = P.internalModuleSourceSpan "<internal>"
