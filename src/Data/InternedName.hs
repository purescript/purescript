{-# LANGUAGE DerivingStrategies #-}
module Data.InternedName where

import Prelude
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Interned (intern, unintern)
import Data.Interned.Text (InternedText)
import Control.DeepSeq (NFData(..))
import Codec.Serialise (Serialise(..), encode, decode)
import Data.Text as T

newtype InternedName = InternedName InternedText
  deriving stock (Show)
  deriving newtype (Eq, Ord)

internName :: String -> InternedName
internName name = InternedName $ intern $ T.pack name

uninternName :: InternedName -> String
uninternName (InternedName name) = T.unpack $ unintern name

instance FromJSON InternedName where
  parseJSON = fmap internName . parseJSON

instance ToJSON InternedName where
  toJSON = toJSON . uninternName

instance Serialise InternedName where
  encode = encode . uninternName
  decode = fmap internName decode

instance NFData InternedName where
  rnf (InternedName _) = ()
