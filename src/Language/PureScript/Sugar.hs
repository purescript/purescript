-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar
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

module Language.PureScript.Sugar (desugar, module S) where

import Control.Monad

import Language.PureScript.Declarations

import Language.PureScript.Sugar.Operators as S
import Language.PureScript.Sugar.DoNotation as S
import Language.PureScript.Sugar.CaseDeclarations as S
import Language.PureScript.Sugar.TypeDeclarations as S
import Language.PureScript.Sugar.BindingGroups as S

desugar :: [Module] -> Either String [Module]
desugar = rebracket
          >=> desugarDo
          >=> desugarCasesModule
          >=> desugarTypeDeclarationsModule
          >=> return . createBindingGroupsModule
