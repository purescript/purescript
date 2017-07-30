-- |
-- Case binders
--
module Language.PureScript.AST.Binders where

import Prelude.Compat

import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Literals
import Language.PureScript.Names
import Language.PureScript.Types

-- |
-- Data type for binders
--
data Binder
  -- |
  -- Wildcard binder
  --
  = NullBinder SourceSpan
  -- |
  -- A binder which matches a literal
  --
  | LiteralBinder SourceSpan (Literal Binder)
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder SourceSpan Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder SourceSpan (Qualified (ProperName 'ConstructorName)) [Binder]
  -- |
  -- A operator alias binder. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | OpBinder SourceSpan (Qualified (OpName 'ValueOpName))
  -- |
  -- Binary operator application. During the rebracketing phase of desugaring,
  -- this data constructor will be removed.
  --
  | BinaryNoParensBinder SourceSpan Binder Binder Binder
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  --
  | ParensInBinder SourceSpan Binder
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder SourceSpan Ident Binder
  -- |
  -- A binder with a type annotation
  --
  | TypedBinder SourceSpan Type Binder
  deriving (Show, Eq, Ord)

-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: Binder -> [Ident]
binderNames = go []
  where
  go ns (LiteralBinder _ b) = lit ns b
  go ns (VarBinder _ name) = name : ns
  go ns (ConstructorBinder _ _ bs) = foldl go ns bs
  go ns (BinaryNoParensBinder _ b1 b2 b3) = foldl go ns [b1, b2, b3]
  go ns (ParensInBinder _ b) = go ns b
  go ns (NamedBinder _ name b) = go (name : ns) b
  go ns (TypedBinder _ _ b) = go ns b
  go ns _ = ns
  lit ns (ObjectLiteral bs) = foldl go ns (fmap snd bs)
  lit ns (ArrayLiteral bs) = foldl go ns bs
  lit ns _ = ns

isIrrefutable :: Binder -> Bool
isIrrefutable NullBinder{} = True
isIrrefutable (VarBinder _ _) = True
isIrrefutable (TypedBinder _ _ b) = isIrrefutable b
isIrrefutable _ = False

binderSourceSpan :: Binder -> SourceSpan
binderSourceSpan (NullBinder ss) = ss
binderSourceSpan (LiteralBinder ss _) = ss
binderSourceSpan (VarBinder ss _) = ss
binderSourceSpan (ConstructorBinder ss _ _) = ss
binderSourceSpan (OpBinder ss _) = ss
binderSourceSpan (BinaryNoParensBinder ss _ _ _) = ss
binderSourceSpan (ParensInBinder ss _) = ss
binderSourceSpan (NamedBinder ss _ _) = ss
binderSourceSpan (TypedBinder ss _ _) = ss
