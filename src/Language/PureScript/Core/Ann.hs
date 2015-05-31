-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Core.Ann
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

module Language.PureScript.Core.Ann where

import Language.PureScript.AST.SourcePos
import Language.PureScript.Comments
import Language.PureScript.Core.Meta
import Language.PureScript.Types

-- |
-- Type alias for basic annotations
--
type Ann = (Maybe SourceSpan, [Comment], Maybe Type, Maybe Meta)

-- |
-- Initial annotation with no metadata
--
nullAnn :: Ann
nullAnn = (Nothing, [], Nothing, Nothing)

-- |
-- Remove the comments from an annotation
--
removeComments :: Ann -> Ann
removeComments (ss, _, ty, meta) = (ss, [], ty, meta)
