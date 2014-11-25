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

resugar :: Module -> A.Module
resugar (Module mn imps exps foreigns decls) =
  A.Module mn (map importToDecl imps
            ++ map foreignToDecl foreigns
            ++ map bindToDecl decls) (Just $ map A.ValueRef exps)

  where

  exprToAST :: Expr -> A.Expr
  exprToAST (Literal l) = literalToExprAST l
  exprToAST (Accessor name v) = A.Accessor name (exprToAST v)
  exprToAST (ObjectUpdate obj vs) = A.ObjectUpdate (exprToAST obj) $ map (second exprToAST) vs
  exprToAST (Abs name v) = A.Abs (Left name) (exprToAST v)
  exprToAST (App v1 v2) = A.App (exprToAST v1) (exprToAST v2)
  exprToAST (Var ident) = A.Var ident
  exprToAST (Case vs alts) = A.Case (map exprToAST vs) (map altToAST alts)
  exprToAST (TypedValue v ty) = A.TypedValue False (exprToAST v) ty
  exprToAST (Let ds v) = A.Let (map bindToDecl ds) (exprToAST v)
  exprToAST (Constructor _ name arity) =
    let args = [ "value" ++ show index | index <- [0 .. arity - 1] ]
        props = ("$ctor", A.StringLiteral $ runModuleName mn ++ "." ++ runProperName name) : [ (arg, A.Var $ Qualified Nothing (Ident arg)) | arg <- args ]
    in foldl (\e arg -> A.Abs (Left $ Ident arg) e) (A.ObjectLiteral props) args
  exprToAST (Meta _ v) = exprToAST v

  literalToExprAST :: Literal Expr -> A.Expr
  literalToExprAST (NumericLiteral v) = A.NumericLiteral v
  literalToExprAST (StringLiteral v) = A.StringLiteral v
  literalToExprAST (BooleanLiteral v) = A.BooleanLiteral v
  literalToExprAST (ArrayLiteral vs) = A.ArrayLiteral $ map exprToAST vs
  literalToExprAST (ObjectLiteral vs) = A.ObjectLiteral $ map (second exprToAST) vs

  altToAST :: CaseAlternative -> A.CaseAlternative
  altToAST (CaseAlternative bs vs) = A.CaseAlternative (map binderToAST bs) (go vs)
    where
    go :: Either [(Guard, Expr)] Expr -> Either [(A.Guard, A.Expr)] A.Expr
    go (Left ges) = Left $ map (exprToAST *** exprToAST) ges
    go (Right e) = Right (exprToAST e)

  binderToAST :: Binder -> A.Binder
  binderToAST (NullBinder) = A.NullBinder
  binderToAST (LiteralBinder b) = literalToBinderAST b
  binderToAST (VarBinder name) = A.VarBinder name
  binderToAST (ConstructorBinder name bs) = A.ConstructorBinder name (map binderToAST bs)
  binderToAST (ConsBinder b1 b2) = A.ConsBinder (binderToAST b1) (binderToAST b2)
  binderToAST (NamedBinder name b) = A.NamedBinder name (binderToAST b)

  literalToBinderAST :: Literal Binder -> A.Binder
  literalToBinderAST (NumericLiteral v) = A.NumberBinder v
  literalToBinderAST (StringLiteral v) = A.StringBinder v
  literalToBinderAST (BooleanLiteral v) = A.BooleanBinder v
  literalToBinderAST (ArrayLiteral vs) = A.ArrayBinder $ map binderToAST vs
  literalToBinderAST (ObjectLiteral vs) = A.ObjectBinder $ map (second binderToAST) vs

  bindToDecl :: Bind -> A.Declaration
  bindToDecl (NotRec name e) = A.ValueDeclaration name Value [] (Right $ exprToAST e)
  bindToDecl (Rec ds) = A.BindingGroupDeclaration $ map (\(name, e) -> (name, Value, exprToAST e)) ds

  importToDecl :: ModuleName -> A.Declaration
  importToDecl name = A.ImportDeclaration name (A.Qualifying []) Nothing

  foreignToDecl :: ForeignDecl -> A.Declaration
  foreignToDecl (name, Nothing, ty) = A.ExternDeclaration ForeignImport name Nothing ty
  foreignToDecl (name, Just js, ty) = A.ExternDeclaration InlineJavascript name (Just js) ty
