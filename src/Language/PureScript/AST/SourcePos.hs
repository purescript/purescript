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
    -- Source name
    --
    sourceName :: String
    -- |
    -- Line number
    --
  , sourcePosLine :: Int
    -- |
    -- Column number
    --
  , sourcePosColumn :: Int
  } deriving (D.Data, D.Typeable)

instance Show SourcePos where
  show sp = sourceName sp ++ " line " ++ show (sourcePosLine sp) ++ ", column " ++ show (sourcePosColumn sp)
