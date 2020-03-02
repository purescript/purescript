module Language.PureScript.Label (Label(..)) where

import "base-compat" Prelude.Compat hiding (lex)
import "base" GHC.Generics (Generic)
import "deepseq" Control.DeepSeq (NFData)
import "base" Data.Monoid ()
import "base" Data.String (IsString(..))
import qualified "aeson" Data.Aeson as A

import "this" Language.PureScript.PSString (PSString)

-- |
-- Labels are used as record keys and row entry names. Labels newtype PSString
-- because records are indexable by PureScript strings at runtime.
--
newtype Label = Label { runLabel :: PSString }
  deriving (Show, Eq, Ord, IsString, Semigroup, Monoid, A.ToJSON, A.FromJSON, Generic)

instance NFData Label
