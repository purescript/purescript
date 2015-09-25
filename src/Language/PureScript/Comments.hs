-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Comments
-- Copyright   :  (c) Phil Freeman 2015
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Defines the types of source code comments
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.PureScript.Comments where

import Data.Aeson.TH
import qualified Data.Data as D

data Comment
  = LineComment String
  | BlockComment String
  deriving (Show, Read, Eq, Ord, D.Data, D.Typeable)

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Comment)
