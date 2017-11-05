{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE DataKinds         #-}
module Ide.Helper where

import Control.Concurrent.STM
import "monad-logger" Control.Monad.Logger
import Language.PureScript.Ide
import Language.PureScript.Ide.Command
import Language.PureScript.Ide.Error
import Language.PureScript.Ide.Types
import Protolude

defConfig :: IdeConfiguration
defConfig =
  IdeConfiguration
    { confLogLevel = LogNone
    , confOutputPath = "output/"
    , confGlobs = ["src/*.purs"]
    , confEditorMode = False
    }

runIde' :: IdeConfiguration -> IdeState -> [Command] -> IO ([Either IdeError Success], IdeState)
runIde' conf s cs = do
  stateVar <- newTVarIO s
  let env' = IdeEnvironment {ideStateVar = stateVar, ideConfiguration = conf}
  r <- runNoLoggingT (runReaderT (traverse (runExceptT . handleCommand) cs) env')
  newState <- readTVarIO stateVar
  pure (r, newState)

runIde :: [Command] -> IO ([Either IdeError Success], IdeState)
runIde = runIde' defConfig emptyIdeState
