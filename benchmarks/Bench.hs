{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Protolude

import Criterion.Main (defaultMain)
import Ide.Bench (ideBench)

main :: IO ()
main =
  defaultMain [ ideBench ]
