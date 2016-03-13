-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- A collection of pretty printers for core data types:
--
--  [@Language.PureScript.Pretty.Kinds@] Pretty printer for kinds
--
--  [@Language.PureScript.Pretty.Values@] Pretty printer for values
--
--  [@Language.PureScript.Pretty.Types@] Pretty printer for types
--
--  [@Language.PureScript.Pretty.JS@] Pretty printer for values, used for code generation
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty (module P) where

import Language.PureScript.Pretty.Kinds as P
import Language.PureScript.Pretty.Values as P
import Language.PureScript.Pretty.Types as P
import Language.PureScript.Pretty.JS as P
