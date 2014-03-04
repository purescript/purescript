-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.DoNotation
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which replaces do-notation statements with
-- appropriate calls to (>>=) from the Prelude.Monad type class.
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.DoNotation (
    desugarDo
) where

import Data.Data
import Data.Generics

import Language.PureScript.Values
import Language.PureScript.Names
import Language.PureScript.Scope

-- |
-- Replace all @DoNotationBind@ and @DoNotationValue@ constructors with applications of the Prelude.(>>=) function,
-- and all @DoNotationLet@ constructors with let expressions.
--
desugarDo :: (Data d) => d -> Either String d
desugarDo = everywhereM (mkM replace)
  where
  prelude :: ModuleName
  prelude = ModuleName [ProperName "Prelude"]
  ret :: Value
  ret = Var (Qualified (Just prelude) (Ident "ret"))
  bind :: Value
  bind = Var (Qualified (Just prelude) (Op ">>="))
  replace :: Value -> Either String Value
  replace (Do els) = go els
  replace other = return other
  go :: [DoNotationElement] -> Either String Value
  go [] = error "The impossible happened in desugarDo"
  go [DoNotationValue val] = return val
  go (DoNotationValue val : rest) = do
    rest' <- go rest
    return $ App (App bind val) (Abs (Left (Ident "_")) rest')
  go [DoNotationBind _ _] = Left "Bind statement cannot be the last statement in a do block"
  go (DoNotationBind NullBinder val : rest) = go (DoNotationValue val : rest)
  go (DoNotationBind (VarBinder ident) val : rest) = do
    rest' <- go rest
    return $ App (App bind val) (Abs (Left ident) rest')
  go (DoNotationBind binder val : rest) = do
    rest' <- go rest
    let ident = head $ unusedNames rest'
    return $ App (App bind val) (Abs (Left ident) (Case [Var (Qualified Nothing ident)] [CaseAlternative [binder] Nothing rest']))
  go [DoNotationLet _ _] = Left "Let statement cannot be the last statement in a do block"
  go (DoNotationLet binder val : rest) = do
    rest' <- go rest
    return $ Case [val] [CaseAlternative [binder] Nothing rest']
