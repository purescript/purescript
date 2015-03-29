-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- A collection of modules related to code generation:
--
--  [@Language.PureScript.CodeGen.JS@] Code generator for Javascript
--
--  [@Language.PureScript.CodeGen.Externs@] Code generator for extern (foreign import) files
--
--  [@Language.PureScript.CodeGen.Optimize@] Optimization passes for generated Javascript
--
-----------------------------------------------------------------------------

module Language.PureScript.CodeGen (module C) where

import Language.PureScript.CodeGen.Externs as C
