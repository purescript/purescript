-- |
-- Source position information
--
module Language.PureScript.AST.SourcePos where

import Prelude.Compat

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)

-- |
-- Source position information
--
data SourcePos = SourcePos
  { -- |
    -- Line number
    --
    sourcePosLine :: Int
    -- |
    -- Column number
    --
  , sourcePosColumn :: Int
  } deriving (Show, Eq, Ord)

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
  { -- |
    -- Source name
    --
    spanName :: String
    -- |
    -- Start of the span
    --
  , spanStart :: SourcePos
    -- End of the span
    --
  , spanEnd :: SourcePos
  } deriving (Show, Eq, Ord)

displayStartEndPos :: SourceSpan -> Text
displayStartEndPos sp =
  displaySourcePos (spanStart sp) <> " - " <>
  displaySourcePos (spanEnd sp)

displaySourceSpan :: SourceSpan -> Text
displaySourceSpan sp =
  T.pack (spanName sp) <> " " <>
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
