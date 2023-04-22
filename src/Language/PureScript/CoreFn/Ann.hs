module Language.PureScript.CoreFn.Ann where

import Prelude

import Language.PureScript.AST.SourcePos (SourceSpan)
import Language.PureScript.Comments (Comment)
import Language.PureScript.CoreFn.Meta (Meta)
import Language.PureScript.Types (SourceType)

-- |
-- Type alias for basic annotations
--
type Ann = (SourceSpan, [Comment], Maybe SourceType, Maybe Meta)

-- |
-- An annotation empty of metadata aside from a source span.
--
ssAnn :: SourceSpan -> Ann
ssAnn ss = (ss, [], Nothing, Nothing)

-- |
-- Remove the comments from an annotation
--
removeComments :: Ann -> Ann
removeComments (ss, _, ty, meta) = (ss, [], ty, meta)
