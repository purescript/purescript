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
-- Desugaring passes
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar (desugar, toCoreFn, module S) where

import Control.Arrow ((***), second)
import Control.Monad
import Control.Category ((>>>))
import Control.Monad.Trans.Class

import qualified Language.PureScript.AST as A
import qualified Language.PureScript.CoreFn as C
import Language.PureScript.Errors
import Language.PureScript.Supply
import Language.PureScript.Names

import Language.PureScript.Sugar.Operators as S
import Language.PureScript.Sugar.DoNotation as S
import Language.PureScript.Sugar.CaseDeclarations as S
import Language.PureScript.Sugar.TypeDeclarations as S
import Language.PureScript.Sugar.BindingGroups as S
import Language.PureScript.Sugar.TypeClasses as S
import Language.PureScript.Sugar.Names as S

-- |
-- The desugaring pipeline proceeds as follows:
--
--  * Introduce type synonyms for type class dictionaries
--
--  * Rebracket user-defined binary operators
--
--  * Desugar do-notation using the @Prelude.Monad@ type class
--
--  * Desugar top-level case declarations into explicit case expressions
--
--  * Desugar type declarations into value declarations with explicit type annotations
--
--  * Group mutually recursive value and data declarations into binding groups.
--
--  * Qualify any unqualified names and types
--
desugar :: [A.Module] -> SupplyT (Either ErrorStack) [A.Module]
desugar = map removeSignedLiterals
          >>> mapM desugarDoModule
          >=> desugarCasesModule
          >=> lift . (desugarTypeDeclarationsModule
                      >=> desugarImports
                      >=> rebracket)
          >=> desugarTypeClasses
          >=> lift . createBindingGroupsModule

toCoreFn :: A.Expr -> C.Expr
toCoreFn (A.NumericLiteral v) = C.Literal (C.NumericLiteral v)
toCoreFn (A.StringLiteral v) = C.Literal (C.StringLiteral v)
toCoreFn (A.BooleanLiteral v) = C.Literal (C.BooleanLiteral v)
toCoreFn (A.ArrayLiteral vs) = C.Literal (C.ArrayLiteral $ map toCoreFn vs)
toCoreFn (A.ObjectLiteral vs) = C.Literal (C.ObjectLiteral $ map (second toCoreFn) vs)
toCoreFn (A.Accessor name v) = C.Accessor name (toCoreFn v)
toCoreFn (A.ObjectUpdate obj vs) = C.ObjectUpdate (toCoreFn obj) $ map (second toCoreFn) vs
toCoreFn (A.Abs (Left name) v) = C.Abs name (toCoreFn v)
toCoreFn (A.Abs _ _) = error "Abs with Binder argument was not desugared before toCoreFn"
toCoreFn (A.App v1 v2) = C.App (toCoreFn v1) (toCoreFn v2)
toCoreFn (A.Var ident) = C.Var ident
toCoreFn (A.IfThenElse v1 v2 v3) =
    C.Case [toCoreFn v1]
      [ C.CaseAlternative [C.LiteralBinder $ C.BooleanLiteral True] (Right $ toCoreFn v2)
      , C.CaseAlternative [C.LiteralBinder $ C.BooleanLiteral False] (Right $ toCoreFn v3) ]
toCoreFn (A.Constructor name) = C.Constructor name
toCoreFn (A.Case vs alts) = C.Case (map toCoreFn vs) (map altToCoreFn alts)
toCoreFn (A.TypedValue _ v ty) = C.TypedValue (toCoreFn v) ty
toCoreFn (A.Let ds v) = C.Let (map declToCoreFn ds) (toCoreFn v)
toCoreFn (A.TypeClassDictionaryConstructorApp name v) = C.TypeClassDictionaryConstructorApp name (toCoreFn v)
toCoreFn (A.PositionedValue _ v) = toCoreFn v
toCoreFn (A.TypeClassDictionary{}) = error "TypeClassDictionary was not desugared before toCoreFn"
toCoreFn (A.SuperClassDictionary{}) = error "SuperClassDictionary was not desugared before toCoreFn"
toCoreFn (A.Do{}) = error "Do was not desugared before toCoreFn"
toCoreFn (A.UnaryMinus{}) = error "UnaryMinus was not desugared before toCoreFn"
toCoreFn (A.BinaryNoParens{}) = error "BinaryNoParens was not desugared before toCoreFn"
toCoreFn (A.Parens{}) = error "Parens was not desugared before toCoreFn"

altToCoreFn :: A.CaseAlternative -> C.CaseAlternative
altToCoreFn (A.CaseAlternative bs vs) = C.CaseAlternative (map binderToCoreFn bs) (go vs)
  where
  go :: Either [(A.Guard, A.Expr)] A.Expr -> Either [(C.Guard, C.Expr)] C.Expr
  go (Left ges) = Left $ map (toCoreFn *** toCoreFn) ges
  go (Right e) = Right (toCoreFn e)

binderToCoreFn :: A.Binder -> C.Binder
binderToCoreFn (A.NullBinder) = C.NullBinder
binderToCoreFn (A.BooleanBinder b) = C.LiteralBinder (C.BooleanLiteral b)
binderToCoreFn (A.StringBinder s) = C.LiteralBinder (C.StringLiteral s)
binderToCoreFn (A.NumberBinder n) = C.LiteralBinder (C.NumericLiteral n)
binderToCoreFn (A.VarBinder name) = C.VarBinder name
binderToCoreFn (A.ConstructorBinder name bs) = C.ConstructorBinder name (map binderToCoreFn bs)
binderToCoreFn (A.ObjectBinder bs) = C.LiteralBinder (C.ObjectLiteral $ map (second binderToCoreFn) bs)
binderToCoreFn (A.ArrayBinder bs) = C.LiteralBinder (C.ArrayLiteral $ map binderToCoreFn bs)
binderToCoreFn (A.ConsBinder b1 b2) = C.ConsBinder (binderToCoreFn b1) (binderToCoreFn b2)
binderToCoreFn (A.NamedBinder name b) = C.NamedBinder name (binderToCoreFn b)
binderToCoreFn (A.PositionedBinder _ b) = binderToCoreFn b

declToCoreFn :: A.Declaration -> (Ident, C.Expr)
declToCoreFn (A.ValueDeclaration name _ _ (Right e)) = (name, toCoreFn e)
declToCoreFn (A.PositionedDeclaration _ d) = declToCoreFn d
declToCoreFn d = error $ "Unexpected value in declToCoreFn: " ++ show d
