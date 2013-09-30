-----------------------------------------------------------------------------
--
-- Module      :  PureScript
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module PureScript (
    module PureScript.Values,
    module PureScript.Types,
    module PureScript.Kinds,
    module PureScript.Declarations,
    module PureScript.Parser,
    module PureScript.CodeGen,
    module PureScript.TypeChecker
) where

import PureScript.Values
import PureScript.Types
import PureScript.Kinds
import PureScript.Declarations
import PureScript.Parser
import PureScript.CodeGen
import PureScript.TypeChecker
