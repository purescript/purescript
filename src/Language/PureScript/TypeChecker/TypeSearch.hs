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
  -> Integer
  -> StateT TC.CheckState (SupplyT (WriterT b (Except e))) a
  -> Maybe (a, Environment)
checkInEnvironment env nextVar =
  either (const Nothing) Just
  . runExcept
  . evalWriterT
  . P.evalSupplyT nextVar
  . TC.runCheck' env

evalWriterT :: Monad m => WriterT b m r -> m r
evalWriterT m = liftM fst (runWriterT m)

checkSubsume
  :: P.Environment
  -- ^ The Environment which contains the relevant definitions and typeclasses
  -> Integer
  -> P.Type
  -- ^ The user supplied type
  -> P.Type
  -- ^ The type supplied by the environment
  -> Maybe ((P.Expr, [(P.Ident, P.Constraint)]), P.Environment)
checkSubsume env nextVar userT envT = checkInEnvironment env nextVar $ do
  let initializeSkolems =
        Skolem.introduceSkolemScope
        <=< P.replaceAllTypeSynonyms
        <=< P.replaceTypeWildcards

  userT' <- initializeSkolems userT
  envT' <- initializeSkolems envT

  let dummyExpression = P.Var (P.Qualified Nothing (P.Ident "x"))

  elab <- subsumes envT' userT'
  subst <- gets TC.checkSubstitution
  let expP = P.overTypes (P.substituteType subst) (elab dummyExpression)
  Entailment.replaceTypeClassDictionaries False expP

typeSearch
  :: P.Environment
  -> Integer
  -> P.Type
  -> Map (P.Qualified P.Ident) P.Type
typeSearch env nextVar type' =
  Map.mapMaybe (\(x, _, _) -> checkSubsume env nextVar type' x $> x) (P.names env)
