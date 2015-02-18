-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Warnings
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
--

module Language.PureScript.Warnings where

import Control.Monad.Trans.Writer.Strict

import Language.PureScript.Errors

type CompileWarning = CompileError

-- |
-- Type for writing warnings
--
type WarningWriter = WriterT [CompileWarning] IO
