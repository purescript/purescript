-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn.Ann
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | Type alias for basic annotations
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreFn.Ann where

import Language.PureScript.AST.SourcePos
import Language.PureScript.CoreFn.Meta
import Language.PureScript.Types

-- |
-- Type alias for basic annotations
--
type Ann = (Maybe SourcePos, Maybe Type, Maybe Meta)
