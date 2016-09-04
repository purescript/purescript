-- |
-- Functions relating to skolemization used during typechecking
--
module Language.PureScript.TypeChecker.Skolems
  ( skolemize
  , skolemizeTypesInValue
  , skolemEscapeCheck
  ) where

import           Prelude.Compat
import Debug.Trace
import           Control.Applicative ((<|>))
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.State.Class (MonadState(..), gets)

import           Data.Foldable (for_)
import           Data.Functor.Identity (Identity(), runIdentity)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import           Language.PureScript.AST
import           Language.PureScript.Errors
import           Language.PureScript.Traversals (defS)
import           Language.PureScript.TypeChecker.Monad
import           Language.PureScript.Types

-- | Skolemize a type variable by replacing its instances with fresh skolem constants
skolemize :: String -> Int -> Maybe SourceSpan -> Type -> Type
skolemize ident sko ss = replaceTypeVars ident (Skolem ident sko ss)

-- |
-- This function has one purpose - to skolemize type variables appearing in a
-- SuperClassDictionary placeholder. These type variables are somewhat unique since they are the
-- only example of scoped type variables.
--
skolemizeTypesInValue :: String -> Int -> Maybe SourceSpan -> Expr -> Expr
skolemizeTypesInValue ident sko ss = runIdentity . f where
  (_, f, _, _, _) = everywhereWithContextOnValuesM [] defS onExpr onBinder defS defS

  onExpr :: [String] -> Expr -> Identity ([String], Expr)
  onExpr sco (SuperClassDictionary c ts)
    | ident `notElem` sco = return (sco, SuperClassDictionary c (map (skolemize ident sko ss) ts))
  onExpr sco (TypedValue check val ty)
    | ident `notElem` sco = return (sco ++ peelTypeVars ty, TypedValue check val (skolemize ident sko ss ty))
  onExpr sco other = return (sco, other)

  onBinder :: [String] -> Binder -> Identity ([String], Binder)
  onBinder sco (TypedBinder ty b)
    | ident `notElem` sco = return (sco ++ peelTypeVars ty, TypedBinder (skolemize ident sko ss ty) b)
  onBinder sco other = return (sco, other)

  peelTypeVars :: Type -> [String]
  peelTypeVars (ForAll i ty) = i : peelTypeVars ty
  peelTypeVars _ = []

-- |
-- Ensure skolem variables do not escape their scope
--
skolemEscapeCheck :: (MonadState CheckState m, MonadError MultipleErrors m) => m ()
skolemEscapeCheck = do
  skolems <- gets checkSkolems
  for_ (IM.toList skolems) $ \(_, SkolemData{..}) -> do
    let lowCheck = IS.lookupLT skolemLowBound skolemUnifiedWith
        highCheck = skolemHighBound >>= (`IS.lookupGT` skolemUnifiedWith)
    traceShow (skolemLowBound, skolemHighBound, skolemUnifiedWith) $
      for_ (lowCheck <|> highCheck) $ \_ ->
        throwError . singleError $ ErrorMessage [] $ EscapedSkolem Nothing
