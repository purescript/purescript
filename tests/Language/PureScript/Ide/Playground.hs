{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
module Language.PureScript.Ide.Playground where

import           Control.Concurrent.STM
import           "monad-logger" Control.Monad.Logger
import qualified Data.Map                        as Map
import           Language.PureScript.Ide
import qualified Language.PureScript.Ide.Test as Test
import           Language.PureScript.Ide.State
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Usage
import           Protolude
import           System.Directory

import qualified Language.PureScript             as P

pgConfig :: IdeConfiguration
pgConfig =
  IdeConfiguration
    { confLogLevel = LogNone
    , confOutputPath = "output/"
    , confGlobs = ["src/**/*.purs", ".psc-package/psc-0.11.7/*/*/src/**/*.purs"]
    , confEditorMode = False
    }

runIDE
  :: Maybe IdeState
  -> ExceptT IdeError (ReaderT IdeEnvironment (NoLoggingT IO)) a
  -> IO (Either IdeError a, IdeState)
runIDE s action = do
  stateVar <- newTVarIO (fromMaybe emptyIdeState s)
  let env' = IdeEnvironment {ideStateVar = stateVar, ideConfiguration = pgConfig}
  r <- runNoLoggingT (runReaderT (runExceptT action) env')
  newState <- readTVarIO stateVar
  pure (r, newState)

play = map fst $ runIDE Nothing $ do
  liftIO $ setCurrentDirectory "C:\\Users\\creek\\code\\find-usage"
  _ <- handleCommand (LoadSync [])
  ms <- getAllModules Nothing
  asts <- Map.map fst . fsModules <$> getFileState
  -- let elig = eligibleModules (Test.mn "Node.Encoding", _idaDeclaration $ Test.ideDtor "UTF8" "Encoding" Nothing) ms asts
  let elig = eligibleModules (Test.mn "Data.Function", _idaDeclaration $ Test.ideValue "const" Nothing) ms asts
  pure $ Map.mapWithKey (\mn searches ->
                  foldMap (\m ->
                             foldMap (applySearch m) searches) (Map.lookup mn asts)) elig
