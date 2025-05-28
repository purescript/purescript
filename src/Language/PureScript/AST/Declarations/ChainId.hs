module Language.PureScript.AST.Declarations.ChainId
  ( ChainId(..)
  , mkChainId
  ) where

import Prelude
import Language.PureScript.AST.SourcePos qualified as Pos
import Data.InternedName (InternedName, internName)
import Control.DeepSeq (NFData)
import Codec.Serialise (Serialise)
import Data.Aeson (ToJSON, FromJSON)

-- |
-- For a given instance chain, stores the chain's file name and
-- the starting source pos of the first instance in the chain.
-- This data is used to determine which instances are part of
-- the same instance chain.
newtype ChainId = ChainId (InternedName, Pos.SourcePos)
  deriving (Eq, Ord, Show, NFData, Serialise, ToJSON, FromJSON)

mkChainId :: String -> Pos.SourcePos -> ChainId
mkChainId fileName startingSourcePos = ChainId (internName fileName, startingSourcePos)
