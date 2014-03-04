-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.Let
-- Copyright   :  (c) Phil Freeman 2014
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which turns let bindings into function applications.
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.Let (
  desugarLetBindings
) where

import Data.Generics (mkT, everywhere)

import Language.PureScript.Values
import Language.PureScript.Declarations

-- |
-- Turn let bindings into function applications
--
desugarLetBindings :: [Module] -> [Module]
desugarLetBindings = everywhere (mkT go)
  where
  go (Let binder value result) = Case [value] [CaseAlternative [binder] Nothing result]
  go other = other
