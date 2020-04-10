{-# language PackageImports, BlockArguments, LambdaCase #-}
-- stack build --profile --bench --no-run-benchmarks --work-dir .stack-work-profiling

-- EDIT THE HASH (according to stack's output)!
-- .stack-work-profiling\dist\e626a42b\build\ide-bench\ide-bench.exe C:\Users\creek\code\pscid\ +RTS -p -RTS

module Main where

import                Control.Concurrent.STM
import                Control.DeepSeq (deepseq)
import                Control.Monad.Except
import "monad-logger" Control.Monad.Logger
import                Control.Monad.Reader
import                Data.Either (isLeft)
import                Data.IORef
import qualified      Data.List as List
import                Language.PureScript.Ide
import                Language.PureScript.Ide.Command
import                Language.PureScript.Ide.Error
import                Language.PureScript.Ide.Types
import                System.Directory (setCurrentDirectory)
import                System.Environment (getArgs)

defConfig :: IdeConfiguration
defConfig =
  IdeConfiguration
    { confLogLevel = LogNone
    , confOutputPath = "output/"
    , confGlobs = ["src/**/*.purs"]
    }

runIde' :: IdeConfiguration -> IdeState -> [Command] -> IO ([Either IdeError Success], IdeState)
runIde' conf s cs = do
  stateVar <- newTVarIO s
  ts <- newIORef Nothing
  let env' = IdeEnvironment {ideStateVar = stateVar, ideConfiguration = conf, ideCacheDbTimestamp = ts}
  r <- runNoLoggingT (runReaderT (traverse (runExceptT . handleCommand) cs) env')
  newState <- readTVarIO stateVar
  pure (r, newState)

runIde :: [Command] -> IO ([Either IdeError Success], IdeState)
runIde = runIde' defConfig emptyIdeState

main :: IO ()
main = do
  getArgs >>= \case
    [] -> pure ()
    [path] -> setCurrentDirectory path
    _ -> error "Expecting a single argument specifying the directory to run in"
  (results, state) <- runIde [LoadSync []]
  forM_ (List.find isLeft results) \err ->
    error ("Failed to execute load: " ++ show err)
  let vol = ideVolatileState state
  let astData = vsAstData vol
  let declarations = vsDeclarations vol
  astData `deepseq` declarations `deepseq` pure ()
