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
import           Language.PureScript.Names                   as P
import           Language.PureScript.TypeChecker.Skolems     as Skolem
import           Language.PureScript.TypeChecker.Synonyms    as P
import           Language.PureScript.Types                   as P

checkInEnvironment
  :: Environment
  -> StateT TC.CheckState (SupplyT (WriterT b (Except e))) a
  -> Maybe (a, Environment)
checkInEnvironment env =
  either (const Nothing) Just
  . runExcept
  . evalWriterT
  -- TODO: make sure that unification variables don't overlap
  . P.evalSupplyT 0
  . TC.runCheck' env

evalWriterT :: Monad m => WriterT b m r -> m r
evalWriterT m = liftM fst (runWriterT m)

checkSubsume
  :: P.Environment
  -- ^ The Environment which contains the relevant definitions and typeclasses
  -> P.Type
  -- ^ The type supplied by the environment
  -> P.Type
  -- ^ The user supplied type
  -> Maybe ((P.Expr, [(P.Ident, P.Constraint)]), P.Environment)
checkSubsume env x t = checkInEnvironment env $ do
  let initializeSkolems =
        Skolem.introduceSkolemScope
        <=< P.replaceAllTypeSynonyms
        <=< P.replaceTypeWildcards

  x' <- initializeSkolems x
  t' <- initializeSkolems t

  let dummyExpression = P.Var (P.Qualified Nothing (P.Ident "x"))

  elab <- subsumes t' x'
  subst <- gets TC.checkSubstitution
  let expP = P.overTypes (P.substituteType subst) (elab dummyExpression)
  Entailment.replaceTypeClassDictionaries False expP

typeSearch
  :: P.Environment
  -> P.Type
  -> Map (P.Qualified P.Ident) P.Type
typeSearch env type' =
  Map.mapMaybe (\(x, _, _) -> checkSubsume env type' x $> x) (P.names env)
