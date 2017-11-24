{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import PSPrelude

import qualified Data.Text as T

import qualified TestCompiler
import qualified TestDocs
import qualified TestHierarchy
import qualified TestPrimDocs
import qualified TestPsci
import qualified TestPscIde
import qualified TestPscPublish
import qualified TestUtils

import System.IO (hSetEncoding, stdout, stderr, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  heading "Updating support code"
  TestUtils.updateSupportCode
  heading "Main compiler test suite"
  TestCompiler.main
  heading "Documentation test suite"
  TestDocs.main
  heading "Hierarchy test suite"
  TestHierarchy.main
  heading "Prim documentation test suite"
  TestPrimDocs.main
  heading "psc-publish test suite"
  TestPscPublish.main
  heading "psci test suite"
  TestPsci.main
  heading "psc-ide test suite"
  TestPscIde.main

  where
  heading msg = do
    putText ""
    putText $ T.replicate 79 "#"
    putText $ "# " <> msg
    putText $ T.replicate 79 "#"
    putText ""
