-- | The imperative core language
module Language.PureScript.CoreImp (
  module C
) where

import Language.PureScript.CoreImp.AST as C
import Language.PureScript.CoreImp.Optimizer as C
import Language.PureScript.CoreImp.Optimizer.Blocks as C
import Language.PureScript.CoreImp.Optimizer.Common as C
import Language.PureScript.CoreImp.Optimizer.Inliner as C
import Language.PureScript.CoreImp.Optimizer.MagicDo as C
import Language.PureScript.CoreImp.Optimizer.TCO as C
import Language.PureScript.CoreImp.Optimizer.Unused as C
