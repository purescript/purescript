-- |
-- Functions relating to skolemization used during typechecking
--
module Language.PureScript.TypeChecker.Skolems
  ( newSkolemConstant
  , introduceSkolemScope
  , newSkolemScope
  , skolemize
  , skolemizeTypesInValue
  , skolemEscapeCheck
  ) where

import Prelude.Compat

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), gets, modify)

import Data.Functor.Identity (Identity(), runIdentity)
import Data.List (nub, (\\))
import Data.Monoid
import Data.Text (Text)

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Traversals (defS)
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types

-- |
-- Generate a new skolem constant
--
newSkolemConstant :: (MonadState CheckState m) => m Int
newSkolemConstant = do
  s <- gets checkNextSkolem
  modify $ \st -> st { checkNextSkolem = s + 1 }
  return s

-- |
-- Introduce skolem scope at every occurence of a ForAll
--
introduceSkolemScope :: (MonadState CheckState m) => Type -> m Type
introduceSkolemScope = everywhereOnTypesM go
  where
  go (ForAll ident ty Nothing) = ForAll ident ty <$> (Just <$> newSkolemScope)
  go other = return other

-- |
-- Generate a new skolem scope
--
newSkolemScope :: (MonadState CheckState m) => m SkolemScope
newSkolemScope = do
  s <- gets checkNextSkolemScope
  modify $ \st -> st { checkNextSkolemScope = s + 1 }
  return $ SkolemScope s

-- |
-- Skolemize a type variable by replacing its instances with fresh skolem constants
--
skolemize :: Text -> Int -> SkolemScope -> Maybe SourceSpan -> Type -> Type
skolemize ident sko scope ss = replaceTypeVars ident (Skolem ident sko scope ss)

-- |
-- This function has one purpose - to skolemize type variables appearing in a
-- DeferredDictionary placeholder. These type variables are somewhat unique since they are the
-- only example of scoped type variables.
--
skolemizeTypesInValue :: Text -> Int -> SkolemScope -> Maybe SourceSpan -> Expr -> Expr
skolemizeTypesInValue ident sko scope ss =
  let
    (_, f, _, _, _) = everywhereWithContextOnValuesM [] defS onExpr onBinder defS defS
  in runIdentity . f
  where
  onExpr :: [Text] -> Expr -> Identity ([Text], Expr)
  onExpr sco (DeferredDictionary c ts)
    | ident `notElem` sco = return (sco, DeferredDictionary c (map (skolemize ident sko scope ss) ts))
  onExpr sco (TypedValue check val ty)
    | ident `notElem` sco = return (sco ++ peelTypeVars ty, TypedValue check val (skolemize ident sko scope ss ty))
  onExpr sco other = return (sco, other)

  onBinder :: [Text] -> Binder -> Identity ([Text], Binder)
  onBinder sco (TypedBinder ty b)
    | ident `notElem` sco = return (sco ++ peelTypeVars ty, TypedBinder (skolemize ident sko scope ss ty) b)
  onBinder sco other = return (sco, other)

  peelTypeVars :: Type -> [Text]
  peelTypeVars (ForAll i ty _) = i : peelTypeVars ty
  peelTypeVars _ = []

-- |
-- Ensure skolem variables do not escape their scope
--
skolemEscapeCheck :: (MonadError MultipleErrors m) => Expr -> m ()
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
       ((binding, val) : _) -> throwError . singleError $ ErrorMessage [ ErrorInExpression val ] $ EscapedSkolem binding
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
      collect (Skolem _ _ scope _) = [scope]
      collect _ = []
  go scos _ = (scos, [])
  findBindingScope :: SkolemScope -> Maybe Expr
  findBindingScope sco =
    let (_, f, _, _, _) = everythingOnValues mappend (const mempty) go' (const mempty) (const mempty) (const mempty)
    in getFirst $ f root
    where
    go' val@(TypedValue _ _ (ForAll _ _ (Just sco'))) | sco == sco' = First (Just val)
    go' _ = mempty
skolemEscapeCheck _ = internalError "Untyped value passed to skolemEscapeCheck"
