module TestPsci.CommandTest where

import PSPrelude hiding (get)

import Control.Monad.Trans.RWS.Strict (get)
import Language.PureScript.Interactive
import qualified Data.Text as T
import Data.String (String)
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
    ":complete ma" `prints` ""
    ":complete Data.Functor.ma" `prints` (T.unlines (map ("Data.Functor." <> ) ["map", "mapFlipped"]))
    run "import Data.Functor"
    ":complete ma" `prints` (T.unlines ["map", "mapFlipped"])
