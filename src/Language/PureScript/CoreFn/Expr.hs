-- |
-- The core functional representation
--
module Language.PureScript.CoreFn.Expr where

import Prelude.Compat

import Control.Arrow ((***))

import Language.PureScript.AST.Literals
import Language.PureScript.CoreFn.Binders
import Language.PureScript.Names
import Language.PureScript.PSString (PSString)

-- |
-- Data type for expressions and terms
--
data Expr a
  -- |
  -- A literal value
  --
  = Literal a (Literal (Expr a))
  -- |
  -- A data constructor (type name, constructor name, field names)
  --
  | Constructor a (ProperName 'TypeName) (ProperName 'ConstructorName) [Ident]
  -- |
  -- A record property accessor
  --
  | Accessor a PSString (Expr a)
  -- |
  -- Partial record update
  --
  | ObjectUpdate a (Expr a) [(PSString, Expr a)]
  -- |
  -- Function introduction
  --
  | Abs a Ident (Expr a)
  -- |
  -- Function application
  --
  | App a (Expr a) (Expr a)
  -- |
  -- Variable
  --
  | Var a (Qualified Ident)
  -- |
  -- A case expression
  --
  | Case a [Expr a] [CaseAlternative a]
  -- |
  -- A let binding
  --
  | Let a [Bind a] (Expr a)
  deriving (Show, Functor)

-- |
-- A let or module binding.
--
data Bind a
  -- |
  -- Non-recursive binding for a single value
  --
  = NonRec a Ident (Expr a)
  -- |
  -- Mutually recursive binding group for several values
  --
  | Rec [((a, Ident), Expr a)] deriving (Show, Functor)

-- |
-- A guard is just a boolean-valued expression that appears alongside a set of binders
--
type Guard a = Expr a

-- |
-- An alternative in a case statement
--
data CaseAlternative a = CaseAlternative
  { -- |
    -- A collection of binders with which to match the inputs
    --
    caseAlternativeBinders :: [Binder a]
    -- |
    -- The result expression or a collect of guarded expressions
    --
  , caseAlternativeResult :: Either [(Guard a, Expr a)] (Expr a)
  } deriving (Show)

instance Functor CaseAlternative where

  fmap f (CaseAlternative cabs car) = CaseAlternative
    (fmap (fmap f) cabs)
    (either (Left . fmap (fmap f *** fmap f)) (Right . fmap f) car)

-- |
-- Extract the annotation from a term
--
extractAnn :: Expr a -> a
extractAnn (Literal a _) = a
extractAnn (Constructor a _ _ _) = a
extractAnn (Accessor a _ _) = a
extractAnn (ObjectUpdate a _ _) = a
extractAnn (Abs a _ _) = a
extractAnn (App a _ _) = a
extractAnn (Var a _) = a
extractAnn (Case a _ _) = a
extractAnn (Let a _ _) = a


-- |
-- Modify the annotation on a term
--
modifyAnn :: (a -> a) -> Expr a -> Expr a
modifyAnn f (Literal a b)         = Literal (f a) b
modifyAnn f (Constructor a b c d) = Constructor (f a) b c d
modifyAnn f (Accessor a b c)      = Accessor (f a) b c
modifyAnn f (ObjectUpdate a b c)  = ObjectUpdate (f a) b c
modifyAnn f (Abs a b c)           = Abs (f a) b c
modifyAnn f (App a b c)           = App (f a) b c
modifyAnn f (Var a b)             = Var (f a) b
modifyAnn f (Case a b c)          = Case (f a) b c
modifyAnn f (Let a b c)           = Let (f a) b c
