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

desugarDo :: (Data d) => d -> Either String d
desugarDo = everywhereM (mkM replace)
  where
  replace :: Value -> Either String Value
  replace (Do monad els) = go monad els
  replace other = return other
  go :: Value -> [DoNotationElement] -> Either String Value
  go _ [] = error "The impossible happened in desugarDo"
  go monad [DoNotationReturn val] = return $ App (Accessor "ret" monad) [val]
  go _ (DoNotationReturn _ : _) = Left "Return statement must be the last statement in a do block"
  go _ [DoNotationValue val] = return val
  go monad (DoNotationValue val : rest) = do
    rest' <- go monad rest
    return $ App (App (Accessor "bind" monad) [val]) [Abs [Ident "_"] rest']
  go _ [DoNotationBind _ _] = Left "Bind statement cannot be the last statement in a do block"
  go monad (DoNotationBind NullBinder val : rest) = go monad (DoNotationValue val : rest)
  go monad (DoNotationBind (VarBinder ident) val : rest) = do
    rest' <- go monad rest
    return $ App (App (Accessor "bind" monad) [val]) [Abs [ident] rest']
  go monad (DoNotationBind binder val : rest) = do
    rest' <- go monad rest
    let ident = head $ unusedNames rest'
    return $ App (App (Accessor "bind" monad) [val]) [Abs [ident] (Case [Var (Qualified Nothing ident)] [([binder], Nothing, rest')])]
  go _ [DoNotationLet _ _] = Left "Let statement cannot be the last statement in a do block"
  go monad (DoNotationLet binder val : rest) = do
    rest' <- go monad rest
    return $ Case [val] [([binder], Nothing, rest')]
