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

checkInEnvironment
  :: Environment
  -> TC.CheckState
  -> StateT TC.CheckState (SupplyT (WriterT b (Except P.MultipleErrors))) a
  -> Maybe (a, Environment)
checkInEnvironment env st =
  either (const Nothing) Just
  . runExcept
  . evalWriterT
  . P.evalSupplyT 0
  . TC.runCheck' (st { TC.checkEnv = env })

evalWriterT :: Monad m => WriterT b m r -> m r
evalWriterT m = liftM fst (runWriterT m)

checkSubsume
  :: Maybe [(P.Ident, Entailment.InstanceContext, P.Constraint)]
  -- ^ Additional constraints we need to satisfy
  -> P.Environment
  -- ^ The Environment which contains the relevant definitions and typeclasses
  -> TC.CheckState
  -- ^ The typechecker state
  -> P.Type
  -- ^ The user supplied type
  -> P.Type
  -- ^ The type supplied by the environment
  -> Maybe ((P.Expr, [(P.Ident, Entailment.InstanceContext, P.Constraint)]), P.Environment)
checkSubsume unsolved env st userT envT = checkInEnvironment env st $ do
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

  -- Now check that any unsolved constraints have not become impossible
  (traverse_ . traverse_) (\(_, context, constraint) -> do
    let constraint' = P.mapConstraintArgs (map (P.substituteType subst)) constraint
    flip evalStateT Map.empty . evalWriterT $
      Entailment.entails
        (Entailment.SolverOptions
          { solverShouldGeneralize = True
          , solverDeferErrors      = False
          }) constraint' context []) unsolved

  -- Finally, check any constraints which were found during elaboration
  Entailment.replaceTypeClassDictionaries (isJust unsolved) expP

typeSearch
  :: Maybe [(P.Ident, Entailment.InstanceContext, P.Constraint)]
  -- ^ Additional constraints we need to satisfy
  -> P.Environment
  -- ^ The Environment which contains the relevant definitions and typeclasses
  -> TC.CheckState
  -- ^ The typechecker state
  -> P.Type
  -- ^ The type we are looking for
  -> Map (P.Qualified P.Ident) P.Type
typeSearch unsolved env st type' =
  Map.mapMaybe (\(x, _, _) -> checkSubsume unsolved env st type' x $> x) (P.names env)
