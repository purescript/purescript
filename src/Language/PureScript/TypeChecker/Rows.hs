-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker.Rows
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Functions relating to type checking for rows
--
-----------------------------------------------------------------------------

module Language.PureScript.TypeChecker.Rows (
    checkDuplicateLabels
) where

import Data.List

import Control.Monad
import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types

-- |
-- Ensure rows do not contain duplicate labels
--
checkDuplicateLabels :: Expr -> Check ()
checkDuplicateLabels =
  let (_, f, _) = everywhereOnValuesM def go def
  in void . f
  where
  def :: a -> Check a
  def = return

  go :: Expr -> Check Expr
  go e@(TypedValue _ val ty) = do
    checkDups ty
    return e

    where
    checkDups :: Type -> Check ()
    checkDups (TypeApp t1 t2) = checkDups t1 >> checkDups t2
    checkDups (ForAll _ t _) = checkDups t
    checkDups (ConstrainedType args t) = do
      mapM_ checkDups $ concatMap snd args
      checkDups t
    checkDups r@RCons{} =
      let (ls, _) = rowToList r in
      case firstDup . sort . map fst $ ls of
        Just l -> throwError . errorMessage $ DuplicateLabel l (Just val)
        Nothing -> return ()
    checkDups _ = return ()

    firstDup :: (Eq a) => [a] -> Maybe a
    firstDup (x : xs@(x' : _))
      | x == x' = Just x
      | otherwise = firstDup xs
    firstDup _ = Nothing

  go other = return other
