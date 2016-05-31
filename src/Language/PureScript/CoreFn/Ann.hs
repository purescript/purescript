module Language.PureScript.CoreFn.Ann where

import Prelude.Compat

import Language.PureScript.AST.SourcePos
import Language.PureScript.Comments
import Language.PureScript.CoreFn.Meta
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
