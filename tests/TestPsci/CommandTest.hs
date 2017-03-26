module TestPsci.CommandTest where

import Prelude ()
import Prelude.Compat

import Control.Monad.Trans.RWS.Strict (get)
import Language.PureScript.Interactive
import Test.HUnit
import TestPsci.TestEnv

commandTests :: Test
commandTests = TestLabel "commandTests" $ TestList $ map (TestCase . execTestPSCi)
  [ do
      run "import Prelude"
      run "import Data.Functor"
      run "import Control.Monad"
      before <- psciImportedModules <$> get
      length before `equalsTo` 3
      run ":clear"
      after <- psciImportedModules <$> get
      length after `equalsTo` 0
  , do
      run "import Prelude"
      run "import Data.Functor"
      run "import Control.Monad"
      before <- psciImportedModules <$> get
      length before `equalsTo` 3
      run ":reload"
      after <- psciImportedModules <$> get
      length after `equalsTo` 3
  ]
