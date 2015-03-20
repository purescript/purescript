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
-- | The core functional representation
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreFn (
  module C
) where

import Language.PureScript.CoreFn.Binders as C
import Language.PureScript.CoreFn.Desugar as C
import Language.PureScript.CoreFn.Expr as C
import Language.PureScript.CoreFn.Traversals as C
