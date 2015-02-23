-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.AST.SourcePos
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- | Source position information
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module Language.PureScript.AST.SourcePos where

import qualified Data.Data as D

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
  } deriving (Eq, D.Data, D.Typeable)

instance Show SourcePos where
  show sp = "line " ++ show (sourcePosLine sp) ++ ", column " ++ show (sourcePosColumn sp)

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
  } deriving (Eq, D.Data, D.Typeable)

instance Show SourceSpan where
  show sp = spanName sp ++ " " ++ show (spanStart sp) ++ " - " ++ show (spanEnd sp)
