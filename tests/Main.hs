{-# LANGUAGE DoAndIfThenElse #-}

module Main (main) where

import Prelude

import Test.Hspec

import qualified TestAst
import qualified TestCompiler
import qualified TestCoreFn
import qualified TestCst
import qualified TestDocs
import qualified TestHierarchy
import qualified TestPrimDocs
import qualified TestPsci
import qualified TestIde
import qualified TestPscPublish
import qualified TestSourceMaps
-- import qualified TestBundle
import qualified TestMake
import qualified TestUtils
import qualified TestGraph

import System.IO (hSetEncoding, stdout, stderr, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  TestUtils.updateSupportCode

  hspec $ do
    describe "cst" TestCst.spec
    describe "ast" TestAst.spec
    describe "ide" TestIde.spec
    beforeAll TestUtils.setupSupportModules $ do
      describe "compiler" TestCompiler.spec
      describe "sourcemaps" TestSourceMaps.spec
    describe "make" TestMake.spec
    describe "psci" TestPsci.spec
    describe "corefn" TestCoreFn.spec
    describe "docs" TestDocs.spec
    describe "prim-docs" TestPrimDocs.spec
    describe "publish" TestPscPublish.spec
    describe "hierarchy" TestHierarchy.spec
    describe "graph" TestGraph.spec
