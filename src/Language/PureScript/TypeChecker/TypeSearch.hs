{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Language.PureScript.TypeChecker.TypeSearch
  ( typeSearch
  ) where

import           Protolude

import           Control.Monad.Writer
import qualified Data.Map                                    as Map
import qualified Language.PureScript.TypeChecker.Entailment  as Entailment

import qualified Language.PureScript.TypeChecker.Monad       as TC
import           Language.PureScript.TypeChecker.Subsumption
import           Language.PureScript.TypeChecker.Unify       as P

import           Control.Monad.Supply                        as P
import           Language.PureScript.AST                     as P
import           Language.PureScript.Environment             as P
import           Language.PureScript.Errors                  as P
import           Language.PureScript.Names                   as P
import           Language.PureScript.TypeChecker.Skolems     as Skolem
import           Language.PureScript.TypeChecker.Synonyms    as P
import           Language.PureScript.Types                   as P

xrunSubsume
  :: Environment
  -> StateT TC.CheckState (SupplyT (WriterT b (Except e))) a
  -> Either e (a, Environment)
xrunSubsume env = runExcept . evalWriterT . P.evalSupplyT 0 . TC.runCheck' env

evalWriterT :: Monad m => WriterT b m r -> m r
evalWriterT m = liftM fst (runWriterT m)

filtering
  :: P.Environment
  -- ^ The Environment which contains the relevant definitions and typeclasses
  -> P.Type
  -- ^ The type supplied by the environment
  -> P.Type
  -- ^ The user supplied type
  -> Either P.MultipleErrors ((P.Expr, [(P.Ident, P.Constraint)]), P.Environment)
filtering env x t = xrunSubsume env $ do
  let initializeSkolems = Skolem.introduceSkolemScope <=< P.replaceAllTypeSynonyms <=< P.replaceTypeWildcards

  x' <- initializeSkolems x
  t' <- initializeSkolems t

  let dummyExpression = P.Var (P.Qualified Nothing (P.Ident "x"))

  -- We need to add a dummy expression here so that @subsumes@ can succesfully
  -- pattern match on @Just@. It would usually insert dictionaries into the
  -- expression to prove it solved all the constraints, but we don't need to do
  -- any codegen so we're fine with pretending.
  elab <- runExceptT $ subsumes (Just dummyExpression) t' x'
  subst <- gets TC.checkSubstitution
  case elab of
    Right (Just expP) -> do
      let expPP = overTypes (P.substituteType subst) expP
      Entailment.replaceTypeClassDictionaries False expPP
    _ -> throwError undefined

typeSearch
  :: P.Environment
  -> P.Type
  -> Map (P.Qualified P.Ident) P.Type
typeSearch env type' =
  Map.mapMaybe (\(x, _, _) -> if isRight (filtering env type' x)
                              then Just x
                              else Nothing) (P.names env)

overTypes :: (P.Type -> P.Type) -> P.Expr -> P.Expr
overTypes f = let (_, f', _) = P.everywhereOnValues identity g identity in f'
  where
  g :: P.Expr -> P.Expr
  g (P.TypedValue checkTy val t) = P.TypedValue checkTy val (f t)
  g (P.TypeClassDictionary c sco hints) = P.TypeClassDictionary (P.mapConstraintArgs (map f) c) sco hints
  g other = other
