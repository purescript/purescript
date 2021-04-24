{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import Test.Hspec

import qualified TestCompiler
import qualified TestCoreFn
import qualified TestCst
import qualified TestDocs
import qualified TestHierarchy
import qualified TestPrimDocs
import qualified TestPsci
import qualified TestIde
import qualified TestPscPublish
import qualified TestBundle
import qualified TestMake
import qualified TestUtils
import qualified TestGraph

import System.IO (hSetEncoding, stdout, stderr, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  heading "Updating support code"
  TestUtils.updateSupportCode

  hspec $ do
    describe "cst" TestCst.spec
    describe "ide" TestIde.spec
    describe "compiler" TestCompiler.spec
    describe "make" TestMake.spec
    describe "psci" TestPsci.spec
    describe "bundle" TestBundle.spec
    describe "corefn" TestCoreFn.spec
    describe "docs" TestDocs.spec
    describe "prim-docs" TestPrimDocs.spec
    describe "publish" TestPscPublish.spec
    describe "hierarchy" TestHierarchy.spec
    describe "graph" TestGraph.spec
  where
  heading msg = do
    putStrLn ""
    putStrLn $ replicate 79 '#'
    putStrLn $ "# " ++ msg
    putStrLn $ replicate 79 '#'
    putStrLn ""
