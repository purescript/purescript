{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Compiler.Bench (compilerBench) where

import           Protolude

import           Criterion.Main (bench, bgroup, Benchmark, nfIO)
import qualified Criterion.Main as Crit
import qualified Data.Text as T
import           Language.PureScript.Parser.Declarations (parseModulesFromFiles)
import           System.Directory (getCurrentDirectory, setCurrentDirectory)
import           System.FilePath ((</>))
import           System.FilePath.Glob (glob)
import           System.IO.UTF8 (readUTF8FileT)
import           System.Process.Typed (readProcess)

inProject :: IO a -> IO a
inProject f = do
  cwd' <- getCurrentDirectory
  setCurrentDirectory ("benchmarks" </> "project")
  a <- f
  setCurrentDirectory cwd'
  pure a

getSourceGlobs :: IO [FilePath]
getSourceGlobs = do
  (_, out, _) <- readProcess "psc-package sources"
  pure (map toS (T.lines (toS out)))


compilerBench :: Benchmark
compilerBench =
  bgroup "Compiler Benchmarks"
    [ bgroup "Parsing"
      [ Crit.env (inProject (concatMapM glob =<< getSourceGlobs)) $ \inputFiles ->
        bench "Parsing the project sources" $ nfIO $ inProject $ do
          moduleFiles <- forM inputFiles $ \inFile -> (inFile, ) <$> readUTF8FileT inFile
          pure $ either (panic "failed to parse") identity $ runExcept $ parseModulesFromFiles identity moduleFiles
      ]
    ]
