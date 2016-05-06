-- |
-- The core functional representation
--
module Language.PureScript.CoreFn (
  module C
) where

import Language.PureScript.AST.Literals as C
import Language.PureScript.CoreFn.Ann as C
import Language.PureScript.CoreFn.Binders as C
import Language.PureScript.CoreFn.Desugar as C
import Language.PureScript.CoreFn.Expr as C
import Language.PureScript.CoreFn.Meta as C
import Language.PureScript.CoreFn.Module as C
import Language.PureScript.CoreFn.Traversals as C
