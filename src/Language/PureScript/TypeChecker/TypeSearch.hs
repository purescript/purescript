module Language.PureScript.TypeChecker.TypeSearch
  ( typeSearch
  ) where

import           Protolude

import           Control.Monad.Writer (WriterT, runWriterT)
import qualified Data.Map                                    as Map
import qualified Language.PureScript.TypeChecker.Entailment  as Entailment

import qualified Language.PureScript.TypeChecker.Monad       as TC
import           Language.PureScript.TypeChecker.Subsumption
import           Language.PureScript.TypeChecker.Unify       as P

import           Control.Monad.Supply                        as P
import           Language.PureScript.AST                     as P
import           Language.PureScript.Environment             as P
import           Language.PureScript.Errors                  as P
import           Language.PureScript.Label
import           Language.PureScript.Names                   as P
import           Language.PureScript.Pretty.Types            as P
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

accessorSearch
  :: Maybe [(P.Ident, Entailment.InstanceContext, P.Constraint)]
  -> P.Environment
  -> TC.CheckState
  -> P.Type
  -> ([(Label, P.Type)], [(Label, P.Type)])
  -- ^ (all accessors we found, all accessors we found that match the result type)
accessorSearch unsolved env st userT = maybe ([], []) fst $ checkInEnvironment env st $ do
  let initializeSkolems =
        Skolem.introduceSkolemScope
        <=< P.replaceAllTypeSynonyms
        <=< P.replaceTypeWildcards

  userT' <- initializeSkolems userT

  rowType <- freshType
  resultType <- freshType
  let recordFunction = TypeApp (TypeApp tyFunction (TypeApp tyRecord rowType)) resultType
  _ <- subsumes recordFunction userT'
  subst <- gets TC.checkSubstitution
  let solvedRow = fst (rowToList (substituteType subst rowType))
  tcS <- get
  pure (solvedRow, filter (\x -> checkAccessor tcS (substituteType subst resultType) x) solvedRow)
  where
    checkAccessor tcs x (_, type') = isJust (checkSubsume unsolved env tcs x type')

typeSearch
  :: Maybe [(P.Ident, Entailment.InstanceContext, P.Constraint)]
  -- ^ Additional constraints we need to satisfy
  -> P.Environment
  -- ^ The Environment which contains the relevant definitions and typeclasses
  -> TC.CheckState
  -- ^ The typechecker state
  -> P.Type
  -- ^ The type we are looking for
  -> ([(P.Qualified Text, P.Type)], Maybe [(Label, P.Type)])
typeSearch unsolved env st type' =
  let
    runTypeSearch :: Map k P.Type -> Map k P.Type
    runTypeSearch = Map.mapMaybe (\ty -> checkSubsume unsolved env st type' ty $> ty)

    matchingNames = runTypeSearch (Map.map (\(ty, _, _) -> ty) (P.names env))
    matchingConstructors = runTypeSearch (Map.map (\(_, _, ty, _) -> ty) (P.dataConstructors env))
    (allLabels, matchingLabels) = accessorSearch unsolved env st type'
  in
    ( (first (P.Qualified Nothing . ("_." <>) . P.prettyPrintLabel) <$> matchingLabels)
      <> (first (map P.runIdent) <$> Map.toList matchingNames)
      <> (first (map P.runProperName) <$> Map.toList matchingConstructors)
    , if null allLabels then Nothing else Just allLabels)
