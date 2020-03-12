{-# LANGUAGE TemplateHaskell #-}

-- |
-- Defines the types of source code comments
--
module Language.PureScript.Comments where

import "base-compat" Prelude.Compat
import "deepseq" Control.DeepSeq (NFData)
import "text" Data.Text (Text)
import "base" GHC.Generics (Generic)

import "aeson" Data.Aeson.TH

data Comment
  = LineComment Text
  | BlockComment Text
  deriving (Show, Eq, Ord, Generic)

instance NFData Comment

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Comment)
