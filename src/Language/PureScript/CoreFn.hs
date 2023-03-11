-- |
-- The core functional representation
--
module Language.PureScript.CoreFn (
  module C
) where

import Language.PureScript.AST.Literals as C ( Literal(..) )
import Language.PureScript.CoreFn.Ann as C
    ( removeComments, ssAnn, Ann )
import Language.PureScript.CoreFn.Binders as C
    ( extractBinderAnn, Binder(..) )
import Language.PureScript.CoreFn.Desugar as C ( moduleToCoreFn )
import Language.PureScript.CoreFn.Expr as C
    ( extractAnn,
      modifyAnn,
      Bind(..),
      CaseAlternative(..),
      Expr(..),
      Guard )
import Language.PureScript.CoreFn.Meta as C
    ( ConstructorType(..), Meta(..) )
import Language.PureScript.CoreFn.Module as C ( Module(..) )
import Language.PureScript.CoreFn.Optimizer as C ( optimizeCoreFn )
import Language.PureScript.CoreFn.Traversals as C
    ( everywhereOnValues, traverseCoreFn )
