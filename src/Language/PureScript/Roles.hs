{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Data types for roles.
--
module Language.PureScript.Roles
  ( Role(..)
  ) where

import Prelude.Compat

import Control.DeepSeq (NFData)
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import GHC.Generics (Generic)

-- |
-- The role of a type constructor's parameter.
data Role
  = Representational
  -- ^ This parameter's representation affects the representation of the type it
  -- is parameterising.
  | Phantom
  -- ^ This parameter has no effect on the representation of the type it is
  -- parameterising.
  deriving (Show, Eq, Generic)

instance NFData Role

$(A.deriveJSON A.defaultOptions ''Role)
