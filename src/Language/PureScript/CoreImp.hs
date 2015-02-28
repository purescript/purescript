-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The core imperative representation
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreImp (
  module C
) where

import Language.PureScript.CoreFn.Literals as C
import Language.PureScript.CoreFn.Ann as C
import Language.PureScript.CoreFn.Meta as C
import Language.PureScript.CoreImp.AST as C
import Language.PureScript.CoreImp.Desugar as C
import Language.PureScript.CoreImp.Module as C
import Language.PureScript.CoreImp.Operators as C
