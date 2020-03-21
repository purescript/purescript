{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Prelude.Compat

import Test.Tasty

import qualified TestCst

import System.IO (hSetEncoding, stdout, stderr, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  cstTests <- TestCst.main

  defaultMain $
    testGroup
      "Tests"
      [ cstTests
      ]
