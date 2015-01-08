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
    desugarDoModule
) where

import Language.PureScript.Names
import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Supply

import qualified Language.PureScript.Constants as C

import Control.Applicative
import Control.Monad.Trans.Class

-- |
-- Replace all @DoNotationBind@ and @DoNotationValue@ constructors with applications of the Prelude.(>>=) function,
-- and all @DoNotationLet@ constructors with let expressions.
--
desugarDoModule :: Module -> SupplyT (Either ErrorStack) Module
desugarDoModule (Module mn ds exts) = Module mn <$> parU ds desugarDo <*> pure exts

desugarDo :: Declaration -> SupplyT (Either ErrorStack) Declaration
desugarDo (PositionedDeclaration pos com d) = PositionedDeclaration pos com <$> (rethrowWithPosition pos $ desugarDo d)
desugarDo d =
  let (f, _, _) = everywhereOnValuesM return replace return
  in f d
  where
  prelude :: ModuleName
  prelude = ModuleName [ProperName C.prelude]

  bind :: Expr
  bind = Var (Qualified (Just prelude) (Op (C.>>=)))

  replace :: Expr -> SupplyT (Either ErrorStack) Expr
  replace (Do els) = go els
  replace (PositionedValue pos com v) = PositionedValue pos com <$> rethrowWithPosition pos (replace v)
  replace other = return other

  go :: [DoNotationElement] -> SupplyT (Either ErrorStack) Expr
  go [] = error "The impossible happened in desugarDo"
  go [DoNotationValue val] = return val
  go (DoNotationValue val : rest) = do
    rest' <- go rest
    return $ App (App bind val) (Abs (Left (Ident C.__unused)) rest')
  go [DoNotationBind _ _] = lift $ Left $ mkErrorStack "Bind statement cannot be the last statement in a do block" Nothing
  go (DoNotationBind NullBinder val : rest) = go (DoNotationValue val : rest)
  go (DoNotationBind (VarBinder ident) val : rest) = do
    rest' <- go rest
    return $ App (App bind val) (Abs (Left ident) rest')
  go (DoNotationBind binder val : rest) = do
    rest' <- go rest
    ident <- Ident <$> freshName
    return $ App (App bind val) (Abs (Left ident) (Case [Var (Qualified Nothing ident)] [CaseAlternative [binder] (Right rest')]))
  go [DoNotationLet _] = lift $ Left $ mkErrorStack "Let statement cannot be the last statement in a do block" Nothing
  go (DoNotationLet ds : rest) = do
    rest' <- go rest
    return $ Let ds rest'
  go (PositionedDoNotationElement pos com el : rest) = rethrowWithPosition pos $ PositionedValue pos com <$> go (el : rest)
