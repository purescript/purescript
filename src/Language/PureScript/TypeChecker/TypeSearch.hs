module Language.PureScript.TypeChecker.TypeSearch
  ( typeSearch
  ) where

import Protolude

import Control.Monad.Writer (WriterT, runWriterT)
import Data.Map qualified as Map
import Language.PureScript.TypeChecker.Entailment qualified as Entailment

import Language.PureScript.TypeChecker.Monad qualified as TC
import Language.PureScript.TypeChecker.Subsumption ( subsumes )
import Language.PureScript.TypeChecker.Unify as P
    ( freshTypeWithKind, replaceTypeWildcards, substituteType )

import Control.Monad.Supply as P ( evalSupplyT, SupplyT )
import Language.PureScript.AST.Declarations as P ( Expr(Var) )
import Language.PureScript.AST.SourcePos as P ( nullSourceSpan )
import Language.PureScript.AST.Traversals as P ( overTypes )
import Language.PureScript.Environment as P
    ( kindRow,
      kindType,
      tyFunction,
      tyRecord,
      Environment(dataConstructors, names) )
import Language.PureScript.Errors as P ( MultipleErrors )
import Language.PureScript.Label ( Label )
import Language.PureScript.Names as P
    ( pattern ByNullSourcePos,
      Ident(Ident),
      ProperName(runProperName),
      Qualified(..) )
import Language.PureScript.Pretty.Types as P ( prettyPrintLabel )
import Language.PureScript.TypeChecker.Skolems as Skolem
    ( introduceSkolemScope )
import Language.PureScript.TypeChecker.Synonyms as P
    ( replaceAllTypeSynonyms )
import Language.PureScript.Types as P
    ( mapConstraintArgs,
      rowToList,
      srcTypeApp,
      RowListItem(RowListItem),
      SourceConstraint,
      SourceType )

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
  . TC.runCheck (st { TC.checkEnv = env })

evalWriterT :: Monad m => WriterT b m r -> m r
evalWriterT m = fmap fst (runWriterT m)

checkSubsume
  :: Maybe [(P.Ident, Entailment.InstanceContext, P.SourceConstraint)]
  -- ^ Additional constraints we need to satisfy
  -> P.Environment
  -- ^ The Environment which contains the relevant definitions and typeclasses
  -> TC.CheckState
  -- ^ The typechecker state
  -> P.SourceType
  -- ^ The user supplied type
  -> P.SourceType
  -- ^ The type supplied by the environment
  -> Maybe ((P.Expr, [(P.Ident, Entailment.InstanceContext, P.SourceConstraint)]), P.Environment)
checkSubsume unsolved env st userT envT = checkInEnvironment env st $ do
  let initializeSkolems =
        Skolem.introduceSkolemScope
        <=< P.replaceAllTypeSynonyms
        <=< P.replaceTypeWildcards

  userT' <- initializeSkolems userT
  envT' <- initializeSkolems envT

  let dummyExpression = P.Var nullSourceSpan (P.Qualified P.ByNullSourcePos (P.Ident "x"))

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
  :: Maybe [(P.Ident, Entailment.InstanceContext, P.SourceConstraint)]
  -> P.Environment
  -> TC.CheckState
  -> P.SourceType
  -> ([(Label, P.SourceType)], [(Label, P.SourceType)])
  -- ^ (all accessors we found, all accessors we found that match the result type)
accessorSearch unsolved env st userT = maybe ([], []) fst $ checkInEnvironment env st $ do
  let initializeSkolems =
        Skolem.introduceSkolemScope
        <=< P.replaceAllTypeSynonyms
        <=< P.replaceTypeWildcards

  userT' <- initializeSkolems userT

  rowType <- freshTypeWithKind (P.kindRow P.kindType)
  resultType <- freshTypeWithKind P.kindType
  let recordFunction = srcTypeApp (srcTypeApp tyFunction (srcTypeApp tyRecord rowType)) resultType
  _ <- subsumes recordFunction userT'
  subst <- gets TC.checkSubstitution
  let solvedRow = toRowPair <$> fst (rowToList (substituteType subst rowType))
  tcS <- get
  pure (solvedRow, filter (\x -> checkAccessor tcS (substituteType subst resultType) x) solvedRow)
  where
    checkAccessor tcs x (_, type') = isJust (checkSubsume unsolved env tcs x type')
    toRowPair (RowListItem _ lbl ty) = (lbl, ty)

typeSearch
  :: Maybe [(P.Ident, Entailment.InstanceContext, P.SourceConstraint)]
  -- ^ Additional constraints we need to satisfy
  -> P.Environment
  -- ^ The Environment which contains the relevant definitions and typeclasses
  -> TC.CheckState
  -- ^ The typechecker state
  -> P.SourceType
  -- ^ The type we are looking for
  -> ([(P.Qualified Text, P.SourceType)], Maybe [(Label, P.SourceType)])
typeSearch unsolved env st type' =
  let
    runTypeSearch :: Map k P.SourceType -> Map k P.SourceType
    runTypeSearch = Map.mapMaybe (\ty -> checkSubsume unsolved env st type' ty $> ty)

    matchingNames = runTypeSearch (Map.map (\(ty, _, _) -> ty) (P.names env))
    matchingConstructors = runTypeSearch (Map.map (\(_, _, ty, _) -> ty) (P.dataConstructors env))
    (allLabels, matchingLabels) = accessorSearch unsolved env st type'

    runPlainIdent (Qualified m (Ident k), v) = Just (Qualified m k, v)
    runPlainIdent _ = Nothing
  in
    ( (first (P.Qualified P.ByNullSourcePos . ("_." <>) . P.prettyPrintLabel) <$> matchingLabels)
      <> mapMaybe runPlainIdent (Map.toList matchingNames)
      <> (first (map P.runProperName) <$> Map.toList matchingConstructors)
    , if null allLabels then Nothing else Just allLabels)
