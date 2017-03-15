module TestPsci.CommandTest where

import Prelude ()
import Prelude.Compat

import Control.Monad.Trans.RWS.Strict (get)
import Language.PureScript.Interactive
import Test.HUnit
import TestPsci.TestEnv

commandTests :: Test
commandTests = TestLabel "commandTests" $ TestList $ map (TestCase . runTestPSCi)
  [ do
      run "import Prelude"
      run "import Data.Functor"
      run "import Control.Monad"
      before <- psciImportedModules <$> get
      length before @?== 3
      run ":clear"
      after <- psciImportedModules <$> get
      length after @?== 0
  , do
      run "import Prelude"
      run "import Data.Functor"
      run "import Control.Monad"
      before <- psciImportedModules <$> get
      length before @?== 3
      run ":reload"
      after <- psciImportedModules <$> get
      length after @?== 3
  , do
      run "import Prelude"
      run "import Data.Array"
      "let fac n = foldl mul 1 (1..n) in fac 10" @?=> "3628800"
  ]
