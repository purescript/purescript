{-# LANGUAGE TemplateHaskell #-}

-- |
-- Defines the types of source code comments
--
module Language.PureScript.Comments where

import Prelude.Compat

import Data.Aeson.TH

data Comment
  = LineComment String
  | BlockComment String
  deriving (Show, Read, Eq, Ord)

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Comment)
