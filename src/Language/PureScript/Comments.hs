{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Defines the types of source code comments
--
module Language.PureScript.Comments where

import Prelude.Compat

import Control.DeepSeq (NFData)
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics (Generic)

data Comment
  = LineComment Text
  | BlockComment Text
  deriving (Show, Eq, Ord, Generic, NFData)

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Comment)
