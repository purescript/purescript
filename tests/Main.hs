{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import Test.Tasty

import qualified TestCompiler
import qualified TestCoreFn
import qualified TestDocs
import qualified TestHierarchy
import qualified TestPrimDocs
import qualified TestPsci
import qualified TestIde
import qualified TestPscPublish
import qualified TestUtils

import System.IO (hSetEncoding, stdout, stderr, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  heading "Updating support code"
  TestUtils.updateSupportCode
  heading "Prim documentation test suite"
  TestPrimDocs.main
  heading "psc-publish test suite"
  TestPscPublish.main

  ideTests <- TestIde.main
  compilerTests <- TestCompiler.main
  psciTests <- TestPsci.main
  coreFnTests <- TestCoreFn.main
  docsTests <- TestDocs.main
  hierarchyTests <- TestHierarchy.main

  defaultMain $
    testGroup
      "Tests"
      [ compilerTests
      , psciTests
      , ideTests
      , coreFnTests
      , docsTests
      , hierarchyTests
      ]

  where
  heading msg = do
    putStrLn ""
    putStrLn $ replicate 79 '#'
    putStrLn $ "# " ++ msg
    putStrLn $ replicate 79 '#'
    putStrLn ""
