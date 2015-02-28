-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module generates code in the simplified Javascript intermediate representation from Purescript code
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.CodeGen.JS (
    module AST,
    module Common,
    moduleToJs
) where

import Data.List ((\\), delete)
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Traversable as T (traverse)

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class

import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.CodeGen.JS.Common as Common
import Language.PureScript.CodeGen.JS.Optimizer
import Language.PureScript.CoreImp.Operators
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.CoreImp as CI

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToJs :: forall m mode. (Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
           => CI.Module CI.Ann -> m [JS]
moduleToJs (CI.Module coms mn imps exps foreigns stmnts) = do
  additional <- asks optionsAdditional
  jsImports <- T.traverse importToJs . delete (ModuleName [ProperName C.prim]) . (\\ [mn]) $ imps
  let foreigns' = mapMaybe (\(_, js, _) -> js) foreigns
  jsDecls <- mapM statmentToJS stmnts
  optimized <- T.traverse optimize jsDecls
  let isModuleEmpty = null exps
  comments <- not <$> asks optionsNoComments
  let strict = JSStringLiteral "use strict"
  let header = if comments && not (null coms) then JSComment coms strict else strict
  let moduleBody = header : jsImports ++ foreigns' ++ optimized
  let exps' = JSObjectLiteral $ map (runIdent &&& JSVar . identToJs) exps
  return $ case additional of
    MakeOptions -> moduleBody ++ [JSAssignment (JSAccessor "exports" (JSVar "module")) exps']
    CompileOptions ns _ _ | not isModuleEmpty ->
      [ JSVariableIntroduction ns
                               (Just (JSBinary Or (JSVar ns) (JSObjectLiteral [])) )
      , JSAssignment (JSAccessor (moduleNameToJs mn) (JSVar ns))
                     (JSApp (JSFunction Nothing [] (JSBlock (moduleBody ++ [JSReturn exps']))) [])
      ]
    _ -> []

  where

  -- |
  -- Generates Javascript code for a module import.
  --
  importToJs :: ModuleName -> m JS
  importToJs mn' = do
    additional <- asks optionsAdditional
    let moduleBody = case additional of
          MakeOptions -> JSApp (JSVar "require") [JSStringLiteral (runModuleName mn')]
          CompileOptions ns _ _ -> JSAccessor (moduleNameToJs mn') (JSVar ns)
    return $ JSVariableIntroduction (moduleNameToJs mn') (Just moduleBody)

  -- |
  -- Generates Javascript code for a variable reference based on a PureScript
  -- identifier. The ident will be mangled if necessary to produce a valid JS
  -- identifier.
  --
  var :: Ident -> JS
  var = JSVar . identToJs

  -- |
  -- Generate Javascript for an accessor based on a PureScript identifier. If
  -- the name is not valid in Javascript (symbol based, reserved name) an
  -- indexer is returned.
  --
  accessor :: Ident -> JS -> JS
  accessor (Ident prop) = accessorString prop
  accessor (Op op) = JSIndexer (JSStringLiteral op)

  accessorString :: String -> JS -> JS
  accessorString prop | identNeedsEscaping prop = JSIndexer (JSStringLiteral prop)
                      | otherwise = JSAccessor prop

  statmentToJS :: CI.Statement CI.Ann -> m JS
  statmentToJS (CI.Expr e) =
    exprToJS e
  statmentToJS (CI.Function _ ident args body) =
    JSFunction (Just $ identToJs ident)
               (identToJs `map` args) .
               JSBlock <$> mapM statmentToJS body
  statmentToJS (CI.VarDecl _ ident expr) =
    JSVariableIntroduction (identToJs ident) . Just <$> exprToJS expr
  statmentToJS (CI.Assignment _ assignee expr) =
    JSAssignment <$> exprToJS assignee <*> exprToJS expr
  statmentToJS (CI.Loop _ cond body) =
    JSWhile <$> exprToJS cond <*> (JSBlock <$> mapM loopStatementToJS body)
  statmentToJS (CI.IfElse _ cond thens (Just elses)) = do
    thens' <- JSBlock <$> mapM statmentToJS thens
    elses' <- JSBlock <$> mapM statmentToJS elses
    JSIfElse <$> exprToJS cond <*> pure thens' <*> pure (Just elses')
  statmentToJS (CI.IfElse _ cond thens Nothing) = do
    thens' <- JSBlock <$> mapM statmentToJS thens
    JSIfElse <$> exprToJS cond <*> pure thens' <*> pure Nothing
  statmentToJS (CI.Return _ expr) =
    JSReturn <$> exprToJS expr
  statmentToJS (CI.Throw _ msg) =
    return . JSThrow . JSUnary JSNew $ JSApp (JSVar "Error") [JSStringLiteral msg]
  statmentToJS (CI.Label _ lbl stmnt) =
    JSLabel lbl <$> statmentToJS stmnt
  statmentToJS (CI.Comment _ coms') =
    return $ JSComment coms' (JSBlock []) -- whoops

  loopStatementToJS :: CI.LoopStatement CI.Ann -> m JS
  loopStatementToJS (CI.Break _ lbl) = return . JSBreak $ fromMaybe "" lbl
  loopStatementToJS (CI.Continue _ lbl) = return . JSContinue $ fromMaybe "" lbl
  loopStatementToJS (CI.Statement s) = statmentToJS s

  exprToJS :: CI.Expr CI.Ann -> m JS
  exprToJS (CI.Literal _ lit) =
    literalToValueJS lit
  exprToJS (CI.Constructor (_, _, _, Just CI.IsNewtype) _ (ProperName ctor) _) =
    return $ JSVariableIntroduction ctor (Just $
                JSObjectLiteral [("create",
                  JSFunction Nothing ["value"]
                    (JSBlock [JSReturn $ JSVar "value"]))])
  exprToJS (CI.Constructor _ _ (ProperName ctor) []) =
    return $ iife ctor [ JSFunction (Just ctor) [] (JSBlock [])
           , JSAssignment (JSAccessor "value" (JSVar ctor))
                (JSUnary JSNew $ JSApp (JSVar ctor) []) ]
  exprToJS (CI.Constructor _ _ (ProperName ctor) fields) =
    let constructor =
          let body = [ JSAssignment (JSAccessor (identToJs f) (JSVar "this")) (var f) | f <- fields ]
          in JSFunction (Just ctor) (identToJs `map` fields) (JSBlock body)
        createFn =
          let body = JSUnary JSNew $ JSApp (JSVar ctor) (var `map` fields)
          in foldr (\f inner -> JSFunction Nothing [identToJs f] (JSBlock [JSReturn inner])) body fields
    in return $ iife ctor [ constructor
                          , JSAssignment (JSAccessor "create" (JSVar ctor)) createFn
                          ]
  exprToJS (CI.Accessor _ prop expr) =
    JSIndexer <$> exprToJS prop <*> exprToJS expr
  exprToJS (CI.Indexer _ index expr) =
    JSIndexer <$> exprToJS index <*> exprToJS expr
  exprToJS e@(CI.AnonFunction (_, _, _, Just CI.IsTypeClassConstructor) _ _) =
    let args = unAbs e
    in return $ JSFunction Nothing (map identToJs args) (JSBlock $ map assign args)
    where
    unAbs :: CI.Expr CI.Ann -> [Ident]
    unAbs (CI.AnonFunction _ [arg] [CI.Return _ val]) = arg : unAbs val
    unAbs _ = []
    assign :: Ident -> JS
    assign name = JSAssignment (accessorString (runIdent name) (JSVar "this"))
                               (var name)
  exprToJS (CI.AnonFunction _ args stmnts') = do
    body <- JSBlock <$> mapM statmentToJS stmnts'
    return $ JSFunction Nothing (identToJs `map` args) body
  exprToJS e@CI.App{} = do
    let (f, args) = unApp e []
    args' <- mapM exprToJS args
    case f of
      CI.Var (_, _, _, Just CI.IsNewtype) _ -> return (head args')
      CI.Var (_, _, _, Just (CI.IsConstructor _ fields)) name | length args == length fields ->
        return $ JSUnary JSNew $ JSApp (qualifiedToJS id name) args'
      CI.Var (_, _, _, Just CI.IsTypeClassConstructor) name ->
        return $ JSUnary JSNew $ JSApp (qualifiedToJS id name) args'
      _ -> flip (foldl (\fn a -> JSApp fn [a])) args' <$> exprToJS f
    where
    unApp :: CI.Expr CI.Ann -> [CI.Expr CI.Ann] -> (CI.Expr CI.Ann, [CI.Expr CI.Ann])
    unApp (CI.App _ val [arg]) args = unApp val (arg : args)
    unApp other args = (other, args)
  exprToJS (CI.Var (_, _, _, Just (CI.IsConstructor _ [])) ident) =
    return $ JSAccessor "value" $ qualifiedToJS id ident
  exprToJS (CI.Var (_, _, _, Just (CI.IsConstructor _ _)) ident) =
    return $ JSAccessor "create" $ qualifiedToJS id ident
  exprToJS (CI.Var _ ident) =
    return $ varToJs ident
  exprToJS (CI.ObjectUpdate _ obj ps) = do
    obj' <- exprToJS obj
    ps' <- mapM (sndM exprToJS) ps
    extendObj obj' ps'
  exprToJS (CI.UnaryOp _ op expr) =
    JSUnary (unaryToJS op) <$> exprToJS expr
  exprToJS (CI.BinaryOp _ op lhs rhs) =
    JSBinary op <$> exprToJS lhs <*> exprToJS rhs
  exprToJS (CI.IsTagOf _ ctor expr) =
    flip JSInstanceOf (qualifiedToJS (Ident . runProperName) ctor) <$> exprToJS expr

  unaryToJS :: UnaryOp -> JSUnaryOp
  unaryToJS Negate = JSNegate
  unaryToJS Not = JSNot
  unaryToJS BitwiseNot = JSBitwiseNot

  iife :: String -> [JS] -> JS
  iife v exprs = JSApp (JSFunction Nothing [] (JSBlock $ exprs ++ [JSReturn $ JSVar v])) []

  literalToValueJS :: CI.Literal (CI.Expr CI.Ann) -> m JS
  literalToValueJS (CI.NumericLiteral n) = return $ JSNumericLiteral n
  literalToValueJS (CI.StringLiteral s) = return $ JSStringLiteral s
  literalToValueJS (CI.BooleanLiteral b) = return $ JSBooleanLiteral b
  literalToValueJS (CI.ArrayLiteral xs) = JSArrayLiteral <$> mapM exprToJS xs
  literalToValueJS (CI.ObjectLiteral ps) = JSObjectLiteral <$> mapM (sndM exprToJS) ps

  -- |
  -- Shallow copy an object.
  --
  extendObj :: JS -> [(String, JS)] -> m JS
  extendObj obj sts = do
    newObj <- freshName
    key <- freshName
    let
      jsKey = JSVar key
      jsNewObj = JSVar newObj
      block = JSBlock (objAssign:copy:extend ++ [JSReturn jsNewObj])
      objAssign = JSVariableIntroduction newObj (Just $ JSObjectLiteral [])
      copy = JSForIn key obj $ JSBlock [JSIfElse cond assign Nothing]
      cond = JSApp (JSAccessor "hasOwnProperty" obj) [jsKey]
      assign = JSBlock [JSAssignment (JSIndexer jsKey jsNewObj) (JSIndexer jsKey obj)]
      stToAssign (s, js) = JSAssignment (JSAccessor s jsNewObj) js
      extend = map stToAssign sts
    return $ JSApp (JSFunction Nothing [] block) []

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a reference to a
  -- variable.
  --
  varToJs :: Qualified Ident -> JS
  varToJs (Qualified Nothing ident) = var ident
  varToJs qual = qualifiedToJS id qual

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a reference to a
  -- variable that may have a qualified name.
  --
  qualifiedToJS :: (a -> Ident) -> Qualified a -> JS
  qualifiedToJS f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = JSVar . runIdent $ f a
  qualifiedToJS f (Qualified (Just mn') a) | mn /= mn' = accessor (f a) (JSVar (moduleNameToJs mn'))
  qualifiedToJS f (Qualified _ a) = JSVar $ identToJs (f a)
