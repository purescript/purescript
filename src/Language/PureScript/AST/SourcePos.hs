{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- |
-- Source position information
--
module Language.PureScript.AST.SourcePos where

import Prelude.Compat

import Codec.Serialise (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson ((.=), (.:))
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
  } deriving (Show, Eq, Ord, Generic, NFData, Serialise)

displaySourcePos :: SourcePos -> Text
displaySourcePos sp =
  "line " <> T.pack (show (sourcePosLine sp)) <>
    ", column " <> T.pack (show (sourcePosColumn sp))

displaySourcePosShort :: SourcePos -> Text
displaySourcePosShort sp =
  T.pack (show (sourcePosLine sp)) <>
    ":" <> T.pack (show (sourcePosColumn sp))

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
  } deriving (Show, Eq, Ord, Generic, NFData, Serialise)

displayStartEndPos :: SourceSpan -> Text
displayStartEndPos sp =
  "(" <>
  displaySourcePos (spanStart sp) <> " - " <>
  displaySourcePos (spanEnd sp) <> ")"

displayStartEndPosShort :: SourceSpan -> Text
displayStartEndPosShort sp =
  displaySourcePosShort (spanStart sp) <> " - " <>
  displaySourcePosShort (spanEnd sp)

displaySourceSpan :: FilePath -> SourceSpan -> Text
displaySourceSpan relPath sp =
  T.pack (makeRelative relPath (spanName sp)) <> ":" <>
    displayStartEndPosShort sp <> " " <>
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

nullSourceSpan :: SourceSpan
nullSourceSpan = internalModuleSourceSpan ""

nullSourceAnn :: SourceAnn
nullSourceAnn = (nullSourceSpan, [])

pattern NullSourceSpan :: SourceSpan
pattern NullSourceSpan = SourceSpan "" (SourcePos 0 0) (SourcePos 0 0)

pattern NullSourceAnn :: SourceAnn
pattern NullSourceAnn = (NullSourceSpan, [])

nonEmptySpan :: SourceAnn -> Maybe SourceSpan
nonEmptySpan (NullSourceSpan, _) = Nothing
nonEmptySpan (ss, _) = Just ss

widenSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
widenSourceSpan NullSourceSpan b = b
widenSourceSpan a NullSourceSpan = a
widenSourceSpan (SourceSpan n1 s1 e1) (SourceSpan n2 s2 e2) =
  SourceSpan n (min s1 s2) (max e1 e2)
  where
  n | n1 == ""  = n2
    | otherwise = n1

widenSourceAnn :: SourceAnn -> SourceAnn -> SourceAnn
widenSourceAnn (s1, _) (s2, _) = (widenSourceSpan s1 s2, [])
