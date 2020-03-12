{-# LANGUAGE TemplateHaskell #-}

-- |
-- Data types for roles.
--
module Language.PureScript.Roles
  ( Role(..)
  ) where

import "base-compat" Prelude.Compat

import "deepseq" Control.DeepSeq (NFData)
import qualified "aeson" Data.Aeson as A
import qualified "aeson" Data.Aeson.TH as A
import "base" GHC.Generics (Generic)

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

$(A.deriveJSON A.defaultOptions ''Role)
