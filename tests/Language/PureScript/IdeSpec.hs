{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.IdeSpec where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Data.List
import qualified Data.Map                      as Map
import           Language.PureScript.Ide
import           Language.PureScript.Ide.Types
import           Test.Hspec

testState :: PscIdeState
testState = PscIdeState (Map.fromList [("Data.Array", []), ("Control.Monad.Eff", [])]) Map.empty Nothing

defaultConfig :: Configuration
defaultConfig =
  Configuration
  {
    confOutputPath = "output/"
  , confDebug = False
  }

spec :: SpecWith ()
spec =
  describe "list" $
    describe "loadedModules" $ do
      it "returns an empty list when no modules are loaded" $ do
        st <- newTVarIO emptyPscIdeState
        result <- runReaderT printModules (PscIdeEnvironment st defaultConfig)
        result `shouldBe` ModuleList []
      it "returns the list of loaded modules" $ do
        st <- newTVarIO testState
        ModuleList result <- runReaderT printModules (PscIdeEnvironment st defaultConfig)
        sort result `shouldBe` sort ["Data.Array", "Control.Monad.Eff"]
