module Language.PureScript.Linter.Wildcards
  ( ignoreWildcardsUnderCompleteTypeSignatures
  ) where

import Protolude hiding (Type)

import Language.PureScript.AST (Binder(..), Declaration, Expr(..), everywhereWithContextOnValues)
import Language.PureScript.Types (Type(..), WildcardData(..), everythingOnTypes, everywhereOnTypes)

-- |
-- Replaces `TypeWildcard _ UnnamedWildcard` with
-- `TypeWildcard _ IgnoredWildcard` in places where we don't want to emit a
-- warning about wildcards.
--
-- The guiding principle here is that a wildcard can be ignored if there is a
-- complete (wildcard-free) type signature on a binding somewhere between the
-- type in which the wildcard occurs and the top level of the module. In
-- particular, this means that top-level signatures containing wildcards are
-- always warnings, and a top-level signature always prevents wildcards on
-- inner bindings from emitting warnings.
--
ignoreWildcardsUnderCompleteTypeSignatures :: Declaration -> Declaration
ignoreWildcardsUnderCompleteTypeSignatures = onDecl
  where
  (onDecl, _, _, _, _, _) = everywhereWithContextOnValues False (,) handleExpr handleBinder (,) (,) (,)

  handleExpr isCovered = \case
    tv@(TypedValue chk v ty)
      | isCovered -> (True, TypedValue chk v $ ignoreWildcards ty)
      | otherwise -> (isComplete ty, tv)
    other -> (isCovered, other)

  handleBinder isCovered = \case
    tb@(TypedBinder ty b)
      | isCovered -> (True, TypedBinder (ignoreWildcards ty) b)
      | otherwise -> (isComplete ty, tb)
    other -> (isCovered, other)

ignoreWildcards :: Type a -> Type a
ignoreWildcards = everywhereOnTypes $ \case
  TypeWildcard a UnnamedWildcard -> TypeWildcard a IgnoredWildcard
  other -> other

isComplete :: Type a -> Bool
isComplete = everythingOnTypes (&&) $ \case
  TypeWildcard{} -> False
  _ -> True
