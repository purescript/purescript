{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.PureScript.Label (Label(..)) where

import PSPrelude

import Data.String (IsString(..))
import qualified Data.Aeson as A

import Language.PureScript.PSString (PSString)

-- |
-- Labels are used as record keys and row entry names. Labels newtype PSString
-- because records are indexable by PureScript strings at runtime.
--
newtype Label = Label { runLabel :: PSString }
  deriving (Show, Eq, Ord, IsString, Monoid, A.ToJSON, A.FromJSON, Generic)

instance NFData Label
