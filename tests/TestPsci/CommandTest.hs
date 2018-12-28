{-# LANGUAGE OverloadedStrings #-}

module TestPsci.CommandTest where

import Prelude ()
import Prelude.Compat

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.RWS.Strict (get)
import Language.PureScript (moduleNameFromString)
import Language.PureScript.Interactive
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)
import Test.Hspec
import TestPsci.TestEnv

specPSCi :: String -> TestPSCi () -> Spec
specPSCi label = specify label . execTestPSCi

commandTests :: Spec
commandTests = context "commandTests" $ do
  specPSCi ":clear" $ do
    run "import Prelude"
    run "import Data.Functor"
    run "import Control.Monad"
    ms <- psciImportedModules <$> get
    length ms `equalsTo` 3
    run ":clear"
    ms' <- psciImportedModules <$> get
    length ms' `equalsTo` 0

  specPSCi ":reload" $ do
    run "import Prelude"
    run "import Data.Functor"
    run "import Control.Monad"
    ms <- psciImportedModules <$> get
    length ms `equalsTo` 3
    run ":reload"
    ms' <- psciImportedModules <$> get
    length ms' `equalsTo` 3

  specPSCi ":complete" $ do
    ":complete ma" `prints` []
    ":complete Data.Functor.ma" `prints` []
    run "import Data.Functor"
    ":complete ma" `prints` unlines ["map", "mapFlipped"]
    run "import Control.Monad as M"
    ":complete M.a" `prints` unlines ["M.ap", "M.apply"]

  specPSCi ":browse" $ do
    ":browse Data.Void" `printed` flip shouldContain "data Void"
    ":browse Data.Void" `printed` flip shouldContain "absurd ::"

  specPSCi ":reload, :browse" $ do
    cwd <- liftIO getCurrentDirectory
    let new = cwd </> "tests" </> "support" </> "psci" </> "Reload.edit"

    ":browse Reload" `printed` flip shouldContain    "reload ::"
    ":browse Reload" `printed` flip shouldNotContain "edited ::"

    simulateModuleEdit (moduleNameFromString "Reload") new $ do
      run ":reload"
      ":browse Reload" `printed` flip shouldNotContain "reload ::"
      ":browse Reload" `printed` flip shouldContain    "edited ::"

    ":browse Mirp" `printed` flip shouldContain "is not valid"
    ":browse Prim" `printed` flip shouldContain "class Partial"

  specPSCi ":print" $ do
    let failMsg = "Unable to set the repl's printing function"
    let interactivePrintModuleShouldBe modName = do
          modName' <- (fst . psciInteractivePrint) <$> get
          modName' `equalsTo` modName

    run "import Prelude"
    ":print Prelude.show" `printed` flip shouldContain failMsg
    interactivePrintModuleShouldBe (moduleNameFromString "PSCI.Support")

    ":print InteractivePrint.unsafeEval" `printed` flip shouldNotContain failMsg
    "(identity :: _ -> _)" `printed` flip shouldContain "[Function]"
    interactivePrintModuleShouldBe (moduleNameFromString "InteractivePrint")
    ":print" `printed` flip shouldContain "InteractivePrint"
