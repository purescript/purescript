-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn.Resugar
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | Temporary CoreFn -> AST resugar step
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreFn.Resugar (resugar) where

import Control.Arrow (second, (***))

import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Literals
import Language.PureScript.CoreFn.Module
import Language.PureScript.Environment
import Language.PureScript.Names
import qualified Language.PureScript.AST as A

resugar :: Module a -> A.Module
resugar (Module mn imps exps foreigns decls) =
  A.Module mn (map importToDecl imps
            ++ map foreignToDecl foreigns
            ++ map bindToDecl decls) (Just $ map A.ValueRef exps)

  where

  exprToAST :: Expr a -> A.Expr
  exprToAST (Literal _ l) = literalToExprAST l
  exprToAST (Accessor _ name v) = A.Accessor name (exprToAST v)
  exprToAST (ObjectUpdate _ obj vs) = A.ObjectUpdate (exprToAST obj) $ map (second exprToAST) vs
  exprToAST (Abs _ name v) = A.Abs (Left name) (exprToAST v)
  exprToAST (App _ v1 v2) = A.App (exprToAST v1) (exprToAST v2)
  exprToAST (Var _ ident) = A.Var ident
  exprToAST (Case _ vs alts) = A.Case (map exprToAST vs) (map altToAST alts)
  exprToAST (TypedValue v ty) = A.TypedValue False (exprToAST v) ty
  exprToAST (Let _ ds v) = A.Let (map bindToDecl ds) (exprToAST v)
  exprToAST (Constructor _ _ name arity) =
    let args = [ "value" ++ show index | index <- [0 .. arity - 1] ]
        props = ("$ctor", A.StringLiteral $ runModuleName mn ++ "." ++ runProperName name) : [ (arg, A.Var $ Qualified Nothing (Ident arg)) | arg <- args ]
    in foldl (\e arg -> A.Abs (Left $ Ident arg) e) (A.ObjectLiteral props) args

  literalToExprAST :: Literal (Expr a) -> A.Expr
  literalToExprAST (NumericLiteral v) = A.NumericLiteral v
  literalToExprAST (StringLiteral v) = A.StringLiteral v
  literalToExprAST (BooleanLiteral v) = A.BooleanLiteral v
  literalToExprAST (ArrayLiteral vs) = A.ArrayLiteral $ map exprToAST vs
  literalToExprAST (ObjectLiteral vs) = A.ObjectLiteral $ map (second exprToAST) vs

  altToAST :: CaseAlternative a -> A.CaseAlternative
  altToAST (CaseAlternative bs vs) = A.CaseAlternative (map binderToAST bs) (go vs)
    where
    go :: Either [(Guard a, Expr a)] (Expr a) -> Either [(A.Guard, A.Expr)] A.Expr
    go (Left ges) = Left $ map (exprToAST *** exprToAST) ges
    go (Right e) = Right (exprToAST e)

  binderToAST :: Binder a -> A.Binder
  binderToAST (NullBinder _) = A.NullBinder
  binderToAST (LiteralBinder _ b) = literalToBinderAST b
  binderToAST (VarBinder _ name) = A.VarBinder name
  binderToAST (ConstructorBinder _ (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Array")) _ [b1, b2]) =
    A.ConsBinder (binderToAST b1) (binderToAST b2)
  binderToAST (ConstructorBinder _ _ name bs) = A.ConstructorBinder name (map binderToAST bs)
  binderToAST (NamedBinder _ name b) = A.NamedBinder name (binderToAST b)

  literalToBinderAST :: Literal (Binder a) -> A.Binder
  literalToBinderAST (NumericLiteral v) = A.NumberBinder v
  literalToBinderAST (StringLiteral v) = A.StringBinder v
  literalToBinderAST (BooleanLiteral v) = A.BooleanBinder v
  literalToBinderAST (ArrayLiteral vs) = A.ArrayBinder $ map binderToAST vs
  literalToBinderAST (ObjectLiteral vs) = A.ObjectBinder $ map (second binderToAST) vs

  bindToDecl :: Bind a -> A.Declaration
  bindToDecl (NonRec name e) = A.ValueDeclaration name Value [] (Right $ exprToAST e)
  bindToDecl (Rec ds) = A.BindingGroupDeclaration $ map (\(name, e) -> (name, Value, exprToAST e)) ds

  importToDecl :: ModuleName -> A.Declaration
  importToDecl name = A.ImportDeclaration name (A.Qualifying []) Nothing

  foreignToDecl :: ForeignDecl -> A.Declaration
  foreignToDecl (name, Nothing, ty) = A.ExternDeclaration ForeignImport name Nothing ty
  foreignToDecl (name, Just js, ty) = A.ExternDeclaration InlineJavascript name (Just js) ty
