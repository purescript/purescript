{-# LANGUAGE TemplateHaskell #-}

-- |
-- Data types for roles.
--
module Language.PureScript.Roles
  ( Role(..)
  ) where

import Prelude.Compat

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
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
