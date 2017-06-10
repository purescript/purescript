{-# LANGUAGE DeriveGeneric #-}
-- |
-- Source position information
--
module Language.PureScript.AST.SourcePos where

import Prelude.Compat

import Control.DeepSeq (NFData)
import Data.Aeson ((.=), (.:))
import Data.Monoid
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.PureScript.Comments
import qualified Data.Aeson as A
import qualified Data.Text as T
import System.FilePath (makeRelative)

-- | Source annotation - position information and comments.
type SourceAnn = (SourceSpan, [Comment])

-- | Source position information
data SourcePos = SourcePos
  { sourcePosLine :: Int
    -- ^ Line number
  , sourcePosColumn :: Int
    -- ^ Column number
  } deriving (Show, Eq, Ord, Generic)

instance NFData SourcePos

displaySourcePos :: SourcePos -> Text
displaySourcePos sp =
  "line " <> T.pack (show (sourcePosLine sp)) <>
    ", column " <> T.pack (show (sourcePosColumn sp))

instance A.ToJSON SourcePos where
  toJSON SourcePos{..} =
    A.toJSON [sourcePosLine, sourcePosColumn]

instance A.FromJSON SourcePos where
  parseJSON arr = do
    [line, col] <- A.parseJSON arr
    return $ SourcePos line col

data SourceSpan = SourceSpan
  { spanName :: String
    -- ^ Source name
  , spanStart :: SourcePos
    -- ^ Start of the span
  , spanEnd :: SourcePos
    -- ^ End of the span
  } deriving (Show, Eq, Ord, Generic)

instance NFData SourceSpan

displayStartEndPos :: SourceSpan -> Text
displayStartEndPos sp =
  displaySourcePos (spanStart sp) <> " - " <>
  displaySourcePos (spanEnd sp)

displaySourceSpan :: FilePath -> SourceSpan -> Text
displaySourceSpan relPath sp =
  T.pack (makeRelative relPath (spanName sp)) <> " " <>
    displayStartEndPos sp

instance A.ToJSON SourceSpan where
  toJSON SourceSpan{..} =
    A.object [ "name"  .= spanName
             , "start" .= spanStart
             , "end"   .= spanEnd
             ]

instance A.FromJSON SourceSpan where
  parseJSON = A.withObject "SourceSpan" $ \o ->
    SourceSpan     <$>
      o .: "name"  <*>
      o .: "start" <*>
      o .: "end"

internalModuleSourceSpan :: String -> SourceSpan
internalModuleSourceSpan name = SourceSpan name (SourcePos 0 0) (SourcePos 0 0)
