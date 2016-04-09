-----------------------------------------------------------------------------
--
-- Module      :  Main
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import qualified TestCompiler
import qualified TestPscPublish
import qualified TestDocs
import qualified TestPsci
import qualified TestPscIde

import System.IO (hSetEncoding, stdout, stderr, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  heading "Main compiler test suite"
  TestCompiler.main
  heading "Documentation test suite"
  TestDocs.main
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
