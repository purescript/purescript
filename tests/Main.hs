{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

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
    putStrLn ""
    putStrLn $ replicate 79 '#'
    putStrLn $ "# " ++ msg
    putStrLn $ replicate 79 '#'
    putStrLn ""
