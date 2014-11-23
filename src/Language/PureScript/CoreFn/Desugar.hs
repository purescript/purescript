-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn.Desugar
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The AST -> CoreFn desugaring step
--
-----------------------------------------------------------------------------

module Language.PureScript.CoreFn.Desugar where

import Control.Arrow (second, (***))

import Language.PureScript.CoreFn.Binders
import Language.PureScript.CoreFn.Expr
import Language.PureScript.CoreFn.Literals
import Language.PureScript.CoreFn.Module
import Language.PureScript.Names
import qualified Language.PureScript.AST as A

moduleToCoreFn :: A.Module -> Module
moduleToCoreFn (A.Module name decls (Just exps)) = undefined
moduleToCoreFn (A.Module{}) = error "Module exports were not elaborated before moduleToCoreFn"

exprToCoreFn :: A.Expr -> Expr
exprToCoreFn (A.NumericLiteral v) = Literal (NumericLiteral v)
exprToCoreFn (A.StringLiteral v) = Literal (StringLiteral v)
exprToCoreFn (A.BooleanLiteral v) = Literal (BooleanLiteral v)
exprToCoreFn (A.ArrayLiteral vs) = Literal (ArrayLiteral $ map exprToCoreFn vs)
exprToCoreFn (A.ObjectLiteral vs) = Literal (ObjectLiteral $ map (second exprToCoreFn) vs)
exprToCoreFn (A.Accessor name v) = Accessor name (exprToCoreFn v)
exprToCoreFn (A.ObjectUpdate obj vs) = ObjectUpdate (exprToCoreFn obj) $ map (second exprToCoreFn) vs
exprToCoreFn (A.Abs (Left name) v) = Abs name (exprToCoreFn v)
exprToCoreFn (A.Abs _ _) = error "Abs with Binder argument was not desugared before exprToCoreFn"
exprToCoreFn (A.App v1 v2) = App (exprToCoreFn v1) (exprToCoreFn v2)
exprToCoreFn (A.Var ident) = Var ident
exprToCoreFn (A.IfThenElse v1 v2 v3) =
  Case [exprToCoreFn v1]
    [ CaseAlternative [LiteralBinder $ BooleanLiteral True] (Right $ exprToCoreFn v2)
    , CaseAlternative [LiteralBinder $ BooleanLiteral False] (Right $ exprToCoreFn v3) ]
exprToCoreFn (A.Constructor name) = Meta IsConstructor (Var $ properToIdent name)
exprToCoreFn (A.Case vs alts) = Case (map exprToCoreFn vs) (map altToCoreFn alts)
exprToCoreFn (A.TypedValue _ v ty) = TypedValue (exprToCoreFn v) ty
exprToCoreFn (A.Let ds v) = Let (map declToCoreFn ds) (exprToCoreFn v)
exprToCoreFn (A.TypeClassDictionaryConstructorApp name v) =
  App (Meta IsTypeClassDictionaryConstructor (Var $ properToIdent name)) (exprToCoreFn v)
exprToCoreFn (A.PositionedValue _ v) = exprToCoreFn v
exprToCoreFn e = error $ "Unexpected value in exprToCoreFn: " ++ show e

altToCoreFn :: A.CaseAlternative -> CaseAlternative
altToCoreFn (A.CaseAlternative bs vs) = CaseAlternative (map binderToCoreFn bs) (go vs)
  where
  go :: Either [(A.Guard, A.Expr)] A.Expr -> Either [(Guard, Expr)] Expr
  go (Left ges) = Left $ map (exprToCoreFn *** exprToCoreFn) ges
  go (Right e) = Right (exprToCoreFn e)

binderToCoreFn :: A.Binder -> Binder
binderToCoreFn (A.NullBinder) = NullBinder
binderToCoreFn (A.BooleanBinder b) = LiteralBinder (BooleanLiteral b)
binderToCoreFn (A.StringBinder s) = LiteralBinder (StringLiteral s)
binderToCoreFn (A.NumberBinder n) = LiteralBinder (NumericLiteral n)
binderToCoreFn (A.VarBinder name) = VarBinder name
binderToCoreFn (A.ConstructorBinder name bs) = ConstructorBinder name (map binderToCoreFn bs)
binderToCoreFn (A.ObjectBinder bs) = LiteralBinder (ObjectLiteral $ map (second binderToCoreFn) bs)
binderToCoreFn (A.ArrayBinder bs) = LiteralBinder (ArrayLiteral $ map binderToCoreFn bs)
binderToCoreFn (A.ConsBinder b1 b2) = ConsBinder (binderToCoreFn b1) (binderToCoreFn b2)
binderToCoreFn (A.NamedBinder name b) = NamedBinder name (binderToCoreFn b)
binderToCoreFn (A.PositionedBinder _ b) = binderToCoreFn b

declToCoreFn :: A.Declaration -> Bind
declToCoreFn (A.ValueDeclaration name _ _ (Right e)) = NotRec name (exprToCoreFn e)
declToCoreFn (A.BindingGroupDeclaration ds) = Rec $ map (\(name, _, e) -> (name, exprToCoreFn e)) ds
declToCoreFn (A.PositionedDeclaration _ d) = declToCoreFn d
declToCoreFn d = error $ "Unexpected value in declToCoreFn: " ++ show d

properToIdent :: Qualified ProperName -> Qualified Ident
properToIdent (Qualified q name) = Qualified q (Ident $ runProperName name)
