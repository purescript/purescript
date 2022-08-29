-- |
-- Case binders
--
module Language.PureScript.AST.Binders where

import Prelude

import Language.PureScript.AST.SourcePos
import Language.PureScript.AST.Literals
import Language.PureScript.Names
import Language.PureScript.Comments
import Language.PureScript.Types

-- |
-- Data type for binders
--
data Binder
  -- |
  -- Wildcard binder
  --
  = NullBinder
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
  | BinaryNoParensBinder Binder Binder Binder
  -- |
  -- Explicit parentheses. During the rebracketing phase of desugaring, this
  -- data constructor will be removed.
  --
  -- Note: although it seems this constructor is not used, it _is_ useful,
  -- since it prevents certain traversals from matching.
  --
  | ParensInBinder Binder
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder SourceSpan Ident Binder
  -- |
  -- A binder with source position information
  --
  | PositionedBinder SourceSpan [Comment] Binder
  -- |
  -- A binder with a type annotation
  --
  | TypedBinder SourceType Binder
  deriving (Show)

-- Manual Eq and Ord instances for `Binder` were added on 2018-03-05. Comparing
-- the `SourceSpan` values embedded in some of the data constructors of `Binder`
-- was expensive. This made exhaustiveness checking observably slow for code
-- such as the `explode` function in `test/purs/passing/LargeSumTypes.purs`.
-- Custom instances were written to skip comparing the `SourceSpan` values. Only
-- the `Ord` instance was needed for the speed-up, but I did not want the `Eq`
-- to have mismatched behavior.
instance Eq Binder where
  NullBinder == NullBinder =
    True
  (LiteralBinder _ lb) == (LiteralBinder _ lb') =
    lb == lb'
  (VarBinder _ ident) == (VarBinder _ ident') =
    ident == ident'
  (ConstructorBinder _ qpc bs) == (ConstructorBinder _ qpc' bs') =
    qpc == qpc' && bs == bs'
  (OpBinder _ qov) == (OpBinder _ qov') =
    qov == qov'
  (BinaryNoParensBinder b1 b2 b3) == (BinaryNoParensBinder b1' b2' b3') =
    b1 == b1' && b2 == b2' && b3 == b3'
  (ParensInBinder b) == (ParensInBinder b') =
    b == b'
  (NamedBinder _ ident b) == (NamedBinder _ ident' b') =
    ident == ident' && b == b'
  (PositionedBinder _ comments b) == (PositionedBinder _ comments' b') =
    comments == comments' && b == b'
  (TypedBinder ty b) == (TypedBinder ty' b') =
    ty == ty' && b == b'
  _ == _ = False

instance Ord Binder where
  compare NullBinder NullBinder = EQ
  compare (LiteralBinder _ lb) (LiteralBinder _ lb') =
    compare lb lb'
  compare (VarBinder _ ident) (VarBinder _ ident') =
    compare ident ident'
  compare (ConstructorBinder _ qpc bs) (ConstructorBinder _ qpc' bs') =
    compare qpc qpc' <> compare bs bs'
  compare (OpBinder _ qov) (OpBinder _ qov') =
    compare qov qov'
  compare (BinaryNoParensBinder b1 b2 b3) (BinaryNoParensBinder b1' b2' b3') =
    compare b1 b1' <> compare b2 b2' <> compare b3 b3'
  compare (ParensInBinder b) (ParensInBinder b') =
    compare b b'
  compare (NamedBinder _ ident b) (NamedBinder _ ident' b') =
    compare ident ident' <> compare b b'
  compare (PositionedBinder _ comments b) (PositionedBinder _ comments' b') =
    compare comments comments' <> compare b b'
  compare (TypedBinder ty b) (TypedBinder ty' b') =
    compare ty ty' <> compare b b'
  compare binder binder' =
    compare (orderOf binder) (orderOf binder')
      where
        orderOf :: Binder -> Int
        orderOf NullBinder = 0
        orderOf LiteralBinder{} = 1
        orderOf VarBinder{} = 2
        orderOf ConstructorBinder{} = 3
        orderOf OpBinder{} = 4
        orderOf BinaryNoParensBinder{} = 5
        orderOf ParensInBinder{} = 6
        orderOf NamedBinder{} = 7
        orderOf PositionedBinder{} = 8
        orderOf TypedBinder{} = 9

-- |
-- Collect all names introduced in binders in an expression
--
binderNames :: Binder -> [Ident]
binderNames = map snd . binderNamesWithSpans

binderNamesWithSpans :: Binder -> [(SourceSpan, Ident)]
binderNamesWithSpans = go []
  where
  go ns (LiteralBinder _ b) = lit ns b
  go ns (VarBinder ss name) = (ss, name) : ns
  go ns (ConstructorBinder _ _ bs) = foldl go ns bs
  go ns (BinaryNoParensBinder b1 b2 b3) = foldl go ns [b1, b2, b3]
  go ns (ParensInBinder b) = go ns b
  go ns (NamedBinder ss name b) = go ((ss, name) : ns) b
  go ns (PositionedBinder _ _ b) = go ns b
  go ns (TypedBinder _ b) = go ns b
  go ns _ = ns
  lit ns (ObjectLiteral bs) = foldl go ns (map snd bs)
  lit ns (ArrayLiteral bs) = foldl go ns bs
  lit ns _ = ns


isIrrefutable :: Binder -> Bool
isIrrefutable NullBinder = True
isIrrefutable (VarBinder _ _) = True
isIrrefutable (PositionedBinder _ _ b) = isIrrefutable b
isIrrefutable (TypedBinder _ b) = isIrrefutable b
isIrrefutable _ = False
