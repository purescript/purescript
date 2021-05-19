module Language.PureScript.AST.Declarations.ChainId
  ( ChainId
  , mkChainId
  ) where

import Prelude
import qualified Language.PureScript.AST.SourcePos as Pos
import Control.DeepSeq (NFData)
import Codec.Serialise (Serialise)

-- |
-- Stores all information needed to determine whether a given
-- instance is a part of an instance chain.
newtype ChainId = ChainId (String, Pos.SourcePos)
  deriving (Eq, Ord, Show, NFData, Serialise)

-- instance Serialise ChainId

mkChainId :: String -> Pos.SourcePos -> ChainId
mkChainId fileName startingSourcePos = ChainId (fileName, startingSourcePos)
