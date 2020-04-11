{-# LANGUAGE TemplateHaskell #-}

-- |
-- Defines the types of source code comments
--
module Language.PureScript.Comments where

import Prelude.Compat
import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Store (Store)

import Data.Aeson.TH

data Comment
  = LineComment Text
  | BlockComment Text
  deriving (Show, Eq, Ord, Generic)

instance NFData Comment
instance Store Comment
instance Serialise Comment

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Comment)
