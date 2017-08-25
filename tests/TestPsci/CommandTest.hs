module TestPsci.CommandTest where

import Prelude ()
import Prelude.Compat

import Control.Monad.Trans.RWS.Strict (get)
import Language.PureScript.Interactive
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
    ":complete Data.Functor.ma" `prints` (unlines (map ("Data.Functor." ++ ) ["map", "mapFlipped"]))
    run "import Data.Functor"
    ":complete ma" `prints` (unlines ["map", "mapFlipped"])
