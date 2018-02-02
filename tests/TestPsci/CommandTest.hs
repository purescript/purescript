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
    let new = cwd </> "examples" </> "psci" </> "Regex.edit"

    ":browse Data.String.Regex" `printed` flip shouldContain    "flags ::"
    ":browse Data.String.Regex" `printed` flip shouldNotContain "psoup ::"

    simulateModuleEdit (moduleNameFromString "Data.String.Regex") new $ do
      run ":reload"
      ":browse Data.String.Regex" `printed` flip shouldNotContain "flags ::"
      ":browse Data.String.Regex" `printed` flip shouldContain    "psoup ::"
