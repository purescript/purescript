module Language.PureScript.AST.Declarations.ChainId
  ( ChainId
  , mkChainId
  ) where

import Prelude
import Language.PureScript.AST.SourcePos qualified as Pos
import Control.DeepSeq (NFData)
import Codec.Serialise (Serialise)
import Data.Aeson (ToJSON, FromJSON (parseJSON), withObject, (.:), object, (.=))
import Data.Interned (intern, unintern)
import Data.Interned.String (InternedString)
import Data.Aeson.Types (ToJSON(..))
import Protolude (NFData(rnf))
import Codec.Serialise.Class (Serialise(encode, decode))

-- |
-- For a given instance chain, stores the chain's file name and
-- the starting source pos of the first instance in the chain.
-- This data is used to determine which instances are part of
-- the same instance chain.
newtype ChainId = ChainId (InternedString, Pos.SourcePos)
  deriving (Eq, Ord, Show)

instance Serialise ChainId where
  encode (ChainId (str, pos)) = encode (unintern str, pos)
  decode = do
    (str, pos) <- decode
    pure $ ChainId (intern str, pos)

instance NFData ChainId where
  rnf (ChainId (internedStr, pos)) =
    rnf (unintern internedStr) `seq` rnf pos

instance FromJSON ChainId where
  parseJSON = withObject "ChainId" $ \v -> do
    str <- v .: "string"
    pos <- v .: "sourcePos"
    return $ ChainId (intern str, pos)

instance ToJSON ChainId where
  toJSON (ChainId (internedStr, pos)) =
    object ["string" .= unintern internedStr, "sourcePos" .= pos]

mkChainId :: String -> Pos.SourcePos -> ChainId
mkChainId fileName startingSourcePos = ChainId (intern fileName, startingSourcePos)
