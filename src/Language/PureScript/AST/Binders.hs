-- |
-- Case binders
--
module Language.PureScript.AST.Binders where

import Prelude.Compat

import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Literals
import Language.PureScript.Names
import Language.PureScript.Comments
import Language.PureScript.Types

-- |
-- Data type for binders
--
data Binder a b
  -- |
  -- Wildcard binder
  --
  = NullBinder b
  -- |
  -- A binder which matches a literal
  --
  | LiteralBinder (Literal (Binder a b)) b
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder Ident b
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder (Qualified (ProperName 'ConstructorName)) [Binder a b] b
  -- |
  -- A operator alias binder. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | OpBinder (Qualified (OpName 'ValueOpName)) b
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | BinaryNoParensBinder (Binder a b) (Binder a b) (Binder a b) b
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  --
  | ParensInBinder (Binder a b) b
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder Ident (Binder a b) b
  -- |
  -- A binder with source position information
  --
  | PositionedBinder SourceSpan [Comment] (Binder a b) b
  -- |
  -- A binder with a type annotation
  --
  | TypedBinder (Type a) (Binder a b) b
  deriving (Show, Functor)

instance Eq (Binder a b) where
  NullBinder                    _ == NullBinder                    _ = True
  LiteralBinder        a1       _ == LiteralBinder        a2       _ = a1 == a2
  VarBinder            a1       _ == VarBinder            a2       _ = a1 == a2
  ConstructorBinder    a1 b1    _ == ConstructorBinder    a2 b2    _ = a1 == a2 && b1 == b2
  OpBinder             a1       _ == OpBinder             a2       _ = a1 == a2
  BinaryNoParensBinder a1 b1 c1 _ == BinaryNoParensBinder a2 b2 c2 _ = a1 == a2 && b1 == b2 && c1 == c2
  ParensInBinder       a1       _ == ParensInBinder       a2       _ = a1 == a2
  NamedBinder          a1 b1    _ == NamedBinder          a2 b2    _ = a1 == a2 && b1 == b2
  PositionedBinder     a1 b1 c1 _ == PositionedBinder     a2 b2 c2 _ = a1 == a2 && b1 == b2 && c1 == c2
  TypedBinder          a1 b1    _ == TypedBinder          a2 b2    _ = a1 == a2 && b1 == b2
  _ == _ = False

-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: Binder a b -> [Ident]
binderNames = go []
  where
  go ns (LiteralBinder b _) = lit ns b
  go ns (VarBinder name _) = name : ns
  go ns (ConstructorBinder _ bs _) = foldl go ns bs
  go ns (BinaryNoParensBinder b1 b2 b3 _) = foldl go ns [b1, b2, b3]
  go ns (ParensInBinder b _) = go ns b
  go ns (NamedBinder name b _) = go (name : ns) b
  go ns (PositionedBinder _ _ b _) = go ns b
  go ns (TypedBinder _ b _) = go ns b
  go ns _ = ns
  lit ns (ObjectLiteral bs) = foldl go ns (map snd bs)
  lit ns (ArrayLiteral bs) = foldl go ns bs
  lit ns _ = ns

isIrrefutable :: Binder a b -> Bool
isIrrefutable NullBinder{} = True
isIrrefutable VarBinder{} = True
isIrrefutable (PositionedBinder _ _ b _) = isIrrefutable b
isIrrefutable (TypedBinder _ b _) = isIrrefutable b
isIrrefutable _ = False
