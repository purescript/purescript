-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.AST.SourcePos
-- Copyright   :  (c) 2013-15 Phil Freeman, (c) 2014-15 Gary Burgess
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Source position information
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}

module Language.PureScript.AST.SourcePos where

import qualified Data.Data as D
import Data.Aeson ((.=))
import qualified Data.Aeson as A

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
  } deriving (Eq, Ord, Show, D.Data, D.Typeable)

displaySourcePos :: SourcePos -> String
displaySourcePos sp =
  "line " ++ show (sourcePosLine sp) ++
    ", column " ++ show (sourcePosColumn sp)

instance A.ToJSON SourcePos where
  toJSON SourcePos{..} =
    A.toJSON [sourcePosLine, sourcePosColumn]

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
  } deriving (Eq, Ord, Show, D.Data, D.Typeable)

displayStartEndPos :: SourceSpan -> String
displayStartEndPos sp =
  displaySourcePos (spanStart sp) ++ " - " ++
  displaySourcePos (spanEnd sp)

displaySourceSpan :: SourceSpan -> String
displaySourceSpan sp =
  spanName sp ++ " " ++
    displayStartEndPos sp

instance A.ToJSON SourceSpan where
  toJSON SourceSpan{..} =
    A.object [ "name"  .= spanName
             , "start" .= spanStart
             , "end"   .= spanEnd
             ]

internalModuleSourceSpan :: String -> SourceSpan
internalModuleSourceSpan name = SourceSpan name (SourcePos 0 0) (SourcePos 0 0)
