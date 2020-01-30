{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import Test.Tasty

import qualified TestCst
import qualified TestCompiler
import qualified TestCoreFn
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

  cstTests <- TestCst.main
  ideTests <- TestIde.main
  compilerTests <- TestCompiler.main
  makeTests <- TestMake.main
  psciTests <- TestPsci.main
  pscBundleTests <- TestBundle.main
  coreFnTests <- TestCoreFn.main
  docsTests <- TestDocs.main
  primDocsTests <- TestPrimDocs.main
  publishTests <- TestPscPublish.main
  hierarchyTests <- TestHierarchy.main
  graphTests <- TestGraph.main

  defaultMain $
    testGroup
      "Tests"
      [ cstTests
      , compilerTests
      , makeTests
      , psciTests
      , pscBundleTests
      , ideTests
      , coreFnTests
      , docsTests
      , primDocsTests
      , publishTests
      , hierarchyTests
      , graphTests
      ]

  where
  heading msg = do
    putStrLn ""
    putStrLn $ replicate 79 '#'
    putStrLn $ "# " ++ msg
    putStrLn $ replicate 79 '#'
    putStrLn ""
