-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker.Skolems
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Functions relating to skolemization used during typechecking
--
-----------------------------------------------------------------------------

module Language.PureScript.TypeChecker.Skolems (
    newSkolemConstant,
    introduceSkolemScope,
    newSkolemScope,
    skolemize,
    skolemizeTypesInValue,
    skolemEscapeCheck
) where

import Data.List (nub, (\\))
import Data.Monoid

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Unify

import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Pretty
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types

-- |
-- Generate a new skolem constant
--
newSkolemConstant :: UnifyT Type Check Int
newSkolemConstant = fresh'

-- |
-- Introduce skolem scope at every occurence of a ForAll
--
introduceSkolemScope :: Type -> UnifyT Type Check Type
introduceSkolemScope = everywhereOnTypesM go
  where
  go (ForAll ident ty Nothing) = ForAll ident ty <$> (Just <$> newSkolemScope)
  go other = return other

-- |
-- Generate a new skolem scope
--
newSkolemScope :: UnifyT Type Check SkolemScope
newSkolemScope = SkolemScope <$> fresh'

-- |
-- Skolemize a type variable by replacing its instances with fresh skolem constants
--
skolemize :: String -> Int -> SkolemScope -> Type -> Type
skolemize ident sko scope = replaceTypeVars ident (Skolem ident sko scope)

-- |
-- This function has one purpose - to skolemize type variables appearing in a
-- SuperClassDictionary placeholder. These type variables are somewhat unique since they are the
-- only example of scoped type variables.
--
skolemizeTypesInValue :: String -> Int -> SkolemScope -> Expr -> Expr
skolemizeTypesInValue ident sko scope = let (_, f, _) = everywhereOnValues id go id in f
  where
  go (SuperClassDictionary c ts) = SuperClassDictionary c (map (skolemize ident sko scope) ts)
  go (TypedValue check val ty) = TypedValue check val (skolemize ident sko scope ty)
  go other = other

-- |
-- Ensure skolem variables do not escape their scope
--
skolemEscapeCheck :: Expr -> Check ()
skolemEscapeCheck (TypedValue False _ _) = return ()
skolemEscapeCheck root@TypedValue{} =
  -- Every skolem variable is created when a ForAll type is skolemized.
  -- This determines the scope of that skolem variable, which is copied from the SkolemScope
  -- field of the ForAll constructor.
  -- We traverse the tree top-down, and collect any SkolemScopes introduced by ForAlls.
  -- If a Skolem is encountered whose SkolemScope is not in the current list, we have found
  -- an escaped skolem variable.
  let (_, f, _, _, _) = everythingWithContextOnValues [] [] (++) def go def def def
  in case f root of
       [] -> return ()
       ((binding, val) : _) -> throwError $ mkErrorStack ("Rigid/skolem type variable " ++ maybe "" (("bound by " ++) . prettyPrintValue) binding ++ " has escaped.") (Just (ExprError val))
  where
  def s _ = (s, [])

  go :: [(SkolemScope, Expr)] -> Expr -> ([(SkolemScope, Expr)], [(Maybe Expr, Expr)])
  go scos val@(TypedValue _ _ (ForAll _ _ (Just sco))) = ((sco, val) : scos, [])
  go scos val@(TypedValue _ _ ty) = case collectSkolems ty \\ map fst scos of
                                      (sco : _) -> (scos, [(findBindingScope sco, val)])
                                      _ -> (scos, [])
    where
    collectSkolems :: Type -> [SkolemScope]
    collectSkolems = nub . everythingOnTypes (++) collect
      where
      collect (Skolem _ _ scope) = [scope]
      collect _ = []
  go scos _ = (scos, [])
  findBindingScope :: SkolemScope -> Maybe Expr
  findBindingScope sco =
    let (_, f, _, _, _) = everythingOnValues mappend (const mempty) go' (const mempty) (const mempty) (const mempty)
    in getFirst $ f root
    where
    go' val@(TypedValue _ _ (ForAll _ _ (Just sco'))) | sco == sco' = First (Just val)
    go' _ = mempty
skolemEscapeCheck val = throwError $ mkErrorStack "Untyped value passed to skolemEscapeCheck" (Just (ExprError val))
