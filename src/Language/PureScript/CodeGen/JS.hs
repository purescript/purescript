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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.CodeGen.JS
  ( module AST
  , module Common
  , moduleToJs
  , mainCall
  ) where

import Data.List ((\\), delete)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List ((\\), delete, intersect)
import qualified Data.Traversable as T (traverse)

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad (replicateM, forM)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class
import Language.PureScript.AST.Declarations (ForeignCode, runForeignCode)
import Language.PureScript.AST.SourcePos
import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.CodeGen.JS.Common as Common
import Language.PureScript.CodeGen.JS.Optimizer
import Language.PureScript.Core
import Language.PureScript.CoreImp.Operators
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.CoreImp.AST as CI

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToJs :: forall m mode. (Applicative m, Monad m, MonadReader (Options mode) m, MonadSupply m)
           => Module (CI.Decl Ann) ForeignCode -> Maybe JS -> m [JS]
moduleToJs (Module coms mn imps exps foreigns decls) foreign = do
  additional <- asks optionsAdditional
  jsImports <- T.traverse importToJs . delete (ModuleName [ProperName C.prim]) . (\\ [mn]) $ imps
  jsDecls <- mapM declToJS decls
  optimized <- T.traverse optimize jsDecls
  let isModuleEmpty = null exps
  comments <- not <$> asks optionsNoComments
  let strict = JSStringLiteral "use strict"
  let header = if comments && not (null coms) then JSComment coms strict else strict
  let foreign' = [JSVariableIntroduction "$foreign" foreign | not $ null foreigns || foreign == Nothing]
  let moduleBody = header : foreign' ++ jsImports ++ optimized
  let foreignExps = exps `intersect` (fst `map` foreigns)
  let standardExps = exps \\ foreignExps
  let exps' = JSObjectLiteral $ map (runIdent &&& JSVar . identToJs) standardExps
                             ++ map (runIdent &&& foreignIdent) foreignExps
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

  declToJS :: CI.Decl Ann -> m JS
  declToJS (CI.VarDecl _ ident expr) =
    JSVariableIntroduction (identToJs ident) . Just <$> exprToJS expr
  declToJS (CI.Function _ ident args body) =
    JSFunction (Just $ identToJs ident)
               (identToJs `map` args) .
               JSBlock <$> mapM statmentToJS body
  declToJS (CI.Constructor (_, _, _, Just IsNewtype) _ ctor _) =
    return $ JSVariableIntroduction (identToJs ctor) (Just $
                JSObjectLiteral [("create",
                  JSFunction Nothing ["value"]
                    (JSBlock [JSReturn $ JSVar "value"]))])
  declToJS (CI.Constructor _ _ ctor []) =
    let ctor' = identToJs ctor
    in return $ iifeDecl ctor' [ JSFunction (Just ctor') [] (JSBlock [])
                               , JSAssignment (JSAccessor "value" (var ctor))
                                              (JSUnary JSNew $ JSApp (var ctor) []) ]
  declToJS (CI.Constructor (_, _, _, meta) _ ctor fields) =
    let constructor =
          let body = [ JSAssignment (accessor f (JSVar "this")) (var f) | f <- fields ]
          in JSFunction (Just $ identToJs ctor) (identToJs `map` fields) (JSBlock body)
        createFn =
          let body = JSUnary JSNew $ JSApp (var ctor) (var `map` fields)
          in foldr (\f inner -> JSFunction Nothing [identToJs f] (JSBlock [JSReturn inner])) body fields
    in return $
      if meta == Just IsTypeClassConstructor
      then constructor
      else iifeDecl (identToJs ctor) [ constructor
                                     , JSAssignment (JSAccessor "create" (var ctor)) createFn
                                     ]

  statmentToJS :: CI.Statement Ann -> m JS
  statmentToJS (CI.Expr e) = exprToJS e
  statmentToJS (CI.Decl d) = declToJS d
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

  loopStatementToJS :: CI.LoopStatement Ann -> m JS
  loopStatementToJS (CI.Break _ lbl) = return . JSBreak $ fromMaybe "" lbl
  loopStatementToJS (CI.Continue _ lbl) = return . JSContinue $ fromMaybe "" lbl
  loopStatementToJS (CI.Statement s) = statmentToJS s

  exprToJS :: CI.Expr Ann -> m JS
  exprToJS (CI.Literal _ lit) =
    literalToValueJS lit
  exprToJS (CI.Accessor _ prop expr) =
    JSIndexer <$> exprToJS prop <*> exprToJS expr
  exprToJS (CI.Indexer _ index expr) =
    JSIndexer <$> exprToJS index <*> exprToJS expr
  exprToJS (CI.AnonFunction _ args stmnts') = do
    body <- JSBlock <$> mapM statmentToJS stmnts'
    return $ JSFunction Nothing (identToJs `map` args) body
  exprToJS (CI.App _ f []) = flip JSApp [] <$> exprToJS f
  exprToJS e@CI.App{} = do
    let (f, args) = unApp e []
    args' <- mapM exprToJS args
    case f of
      CI.Var (_, _, _, Just IsNewtype) _ -> return (head args')
      CI.Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
        return $ JSUnary JSNew $ JSApp (qualifiedToJS id name) args'
      CI.Var (_, _, _, Just IsTypeClassConstructor) name ->
        return $ JSUnary JSNew $ JSApp (qualifiedToJS id name) args'
      _ -> flip (foldl (\fn a -> JSApp fn [a])) args' <$> exprToJS f
    where
    unApp :: CI.Expr Ann -> [CI.Expr Ann] -> (CI.Expr Ann, [CI.Expr Ann])
    unApp (CI.App _ val args1) args2 = unApp val (args1 ++ args2)
    unApp other args = (other, args)
  exprToJS (CI.Var (_, _, _, Just (IsConstructor _ [])) ident) =
    return $ JSAccessor "value" $ qualifiedToJS id ident
  exprToJS (CI.Var (_, _, _, Just (IsConstructor _ _)) ident) =
    return $ JSAccessor "create" $ qualifiedToJS id ident
  exprToJS (CI.Var (_, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
    return $ if mn' == mn
             then foreignIdent ident
             else varToJs qi
  exprToJS (CI.Var (_, _, _, Just IsForeign) ident) =
    error $ "Encountered an unqualified reference to a foreign ident " ++ show ident
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

  iifeDecl :: String -> [JS] -> JS
  iifeDecl v exprs = JSVariableIntroduction v (Just $ iife v exprs)

  literalToValueJS :: Literal (CI.Expr Ann) -> m JS
  literalToValueJS (NumericLiteral n) = return $ JSNumericLiteral n
  literalToValueJS (StringLiteral s) = return $ JSStringLiteral s
  literalToValueJS (CharLiteral c) = return $ JSStringLiteral [c]
  literalToValueJS (BooleanLiteral b) = return $ JSBooleanLiteral b
  literalToValueJS (ArrayLiteral xs) = JSArrayLiteral <$> mapM exprToJS xs
  literalToValueJS (ObjectLiteral ps) = JSObjectLiteral <$> mapM (sndM exprToJS) ps

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

  foreignIdent :: Ident -> JS
  foreignIdent ident = accessorString (runIdent ident) (JSVar "$foreign")

mainCall :: ModuleName -> String -> JS
mainCall mmi ns = JSApp (JSAccessor C.main (JSAccessor (moduleNameToJs mmi) (JSVar ns))) []
