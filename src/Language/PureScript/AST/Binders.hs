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
-- Data type for binders.
--
-- The @a@ parameter allows for types to be annotated  with extra data - source
-- position info, for example, and the @b@ parameter is for value-level
-- annotations. Two separate annotation types are supported as not every
-- annotation for a type would be suitable for a value, and vice-versa.
--
data Binder a b
  -- |
  -- Wildcard binder
  --
  = NullBinder b
  -- |
  -- A binder which matches a literal
  --
  | LiteralBinder b (Literal (Binder a b))
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder b Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder b (Qualified (ProperName 'ConstructorName)) [Binder a b]
  -- |
  -- A operator alias binder. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | OpBinder b (Qualified (OpName 'ValueOpName))
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | BinaryNoParensBinder b (Binder a b) (Binder a b) (Binder a b)
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  --
  | ParensInBinder b (Binder a b)
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder b Ident (Binder a b)
  -- |
  -- A binder with source position information
  --
  | PositionedBinder b SourceSpan [Comment] (Binder a b)
  -- |
  -- A binder with a type annotation
  --
  | TypedBinder b (Type a) (Binder a b)
  deriving (Show, Functor)

instance Eq (Binder a b) where
  NullBinder           _          == NullBinder           _          = True
  LiteralBinder        _ a1       == LiteralBinder        _ a2       = a1 == a2
  VarBinder            _ a1       == VarBinder            _ a2       = a1 == a2
  ConstructorBinder    _ a1 b1    == ConstructorBinder    _ a2 b2    = a1 == a2 && b1 == b2
  OpBinder             _ a1       == OpBinder             _ a2       = a1 == a2
  BinaryNoParensBinder _ a1 b1 c1 == BinaryNoParensBinder _ a2 b2 c2 = a1 == a2 && b1 == b2 && c1 == c2
  ParensInBinder       _ a1       == ParensInBinder       _ a2       = a1 == a2
  NamedBinder          _ a1 b1    == NamedBinder          _ a2 b2    = a1 == a2 && b1 == b2
  PositionedBinder     _ a1 b1 c1 == PositionedBinder     _ a2 b2 c2 = a1 == a2 && b1 == b2 && c1 == c2
  TypedBinder          _ a1 b1    == TypedBinder          _ a2 b2    = a1 == a2 && b1 == b2
  _ == _ = False

-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: Binder a b -> [Ident]
binderNames = go []
  where
  go ns (LiteralBinder _ b) = lit ns b
  go ns (VarBinder _ name) = name : ns
  go ns (ConstructorBinder _ _ bs) = foldl go ns bs
  go ns (BinaryNoParensBinder _ b1 b2 b3) = foldl go ns [b1, b2, b3]
  go ns (ParensInBinder _ b) = go ns b
  go ns (NamedBinder _ name b) = go (name : ns) b
  go ns (PositionedBinder _ _ _ b) = go ns b
  go ns (TypedBinder _ _ b) = go ns b
  go ns _ = ns
  lit ns (ObjectLiteral bs) = foldl go ns (map snd bs)
  lit ns (ArrayLiteral bs) = foldl go ns bs
  lit ns _ = ns

isIrrefutable :: Binder a b -> Bool
isIrrefutable NullBinder{} = True
isIrrefutable VarBinder{} = True
isIrrefutable (PositionedBinder _ _ _ b) = isIrrefutable b
isIrrefutable (TypedBinder _ _ b) = isIrrefutable b
isIrrefutable _ = False
