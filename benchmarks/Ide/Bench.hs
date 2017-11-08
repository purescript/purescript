{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide.Bench (ideBench) where

import           Protolude

import           Criterion.Main (bench, bgroup, Benchmark, nfIO)
import qualified Criterion.Main as Crit
import qualified Data.Text as T
import           Ide.Helper (runIde', defConfig)
import qualified Language.PureScript.Ide.Command as Command
import           Language.PureScript.Ide.Types
import           System.Directory (getCurrentDirectory, setCurrentDirectory)
import           System.FilePath ((</>))
import           System.Process.Typed (readProcess, runProcess)

inProject :: IO a -> IO a
inProject f = do
  cwd' <- getCurrentDirectory
  setCurrentDirectory ("benchmarks" </> "project")
  a <- f
  setCurrentDirectory cwd'
  pure a

compileProject :: IO Bool
compileProject = do
  exitCode <- runProcess "psc-package build"
  pure (isSuccess exitCode)

isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess = True
isSuccess (ExitFailure _) = False

loadWithGlobs :: [FilePath] -> IO (ModuleMap [IdeDeclarationAnn])
loadWithGlobs globs = do
   (_, st) <-
     runIde'
      (defConfig { confGlobs = globs })
      emptyIdeState
      [Command.LoadSync []]
   pure (vsDeclarations (ideVolatileState st))

ideBench :: Benchmark
ideBench =
  Crit.env (inProject compileProject) $ \_ ->
    bgroup "ide"
      [ bench "Without sourceglobs" $ nfIO $ inProject
          $ loadWithGlobs []
      , bench "With sourceglobs" $ nfIO $ inProject
          $ loadWithGlobs [ ".psc-package/psc-0.11.6-09272017/*/*/src/**/*.purs" ]
      ]
