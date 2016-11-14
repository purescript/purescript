{-# LANGUAGE TemplateHaskell #-}

-- |
-- Defines the types of source code comments
--
module Language.PureScript.Comments where

import Prelude.Compat
import Data.Text (Text)

import Data.Aeson.TH

data Comment
  = LineComment Text
  | BlockComment Text
  deriving (Show, Eq, Ord)

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Comment)
