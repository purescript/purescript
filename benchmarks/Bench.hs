{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Criterion.Main (defaultMain)
import Ide.Bench (ideBench)
import Compiler.Bench (compilerBench)

main :: IO ()
main =
  defaultMain [ ideBench, compilerBench ]
