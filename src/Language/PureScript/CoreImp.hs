-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreImp
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

import Language.PureScript.CoreImp.AST as C
import Language.PureScript.CoreImp.Desugar as C
import Language.PureScript.CoreImp.Operators as C
