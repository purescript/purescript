-- |
-- The core functional representation for binders
--
module Language.PureScript.CoreFn.Binders where

import Prelude.Compat

import Language.PureScript.AST.Literals
import Language.PureScript.Names

-- |
-- Data type for binders
--
data Binder a
  -- |
  -- Wildcard binder
  --
  = NullBinder a
  -- |
  -- A binder which matches a literal value
  --
  | LiteralBinder a (Literal (Binder a))
  -- |
  -- A binder which binds an identifier
  --
  | VarBinder a Ident
  -- |
  -- A binder which matches a data constructor
  --
  | ConstructorBinder a (Qualified (ProperName 'TypeName)) (Qualified (ProperName 'ConstructorName)) [Binder a]
  -- |
  -- A binder which binds its input to an identifier
  --
  | NamedBinder a Ident (Binder a) deriving (Show, Functor)


extractBinderAnn :: Binder a -> a
extractBinderAnn (NullBinder a) = a
extractBinderAnn (LiteralBinder a _) = a
extractBinderAnn (VarBinder a _) = a
extractBinderAnn (ConstructorBinder a _ _ _) = a
extractBinderAnn (NamedBinder a _ _) = a
