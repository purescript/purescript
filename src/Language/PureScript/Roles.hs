{-# LANGUAGE TemplateHaskell #-}

-- |
-- Data types for roles.
--
module Language.PureScript.Roles
  ( Role(..)
  , displayRole
  ) where

import Prelude

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson qualified as A
import Data.Aeson.TH qualified as A
import Data.Text (Text)
import GHC.Generics (Generic)

-- |
-- The role of a type constructor's parameter.
data Role
  = Nominal
  -- ^ This parameter's identity affects the representation of the type it is
  -- parameterising.
  | Representational
  -- ^ This parameter's representation affects the representation of the type it
  -- is parameterising.
  | Phantom
  -- ^ This parameter has no effect on the representation of the type it is
  -- parameterising.
  deriving (Show, Eq, Ord, Generic)

instance NFData Role
instance Serialise Role

$(A.deriveJSON A.defaultOptions ''Role)

displayRole :: Role -> Text
displayRole r = case r of
  Nominal -> "nominal"
  Representational -> "representational"
  Phantom -> "phantom"
