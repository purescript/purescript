-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.TypeChecker.Entailment
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Type class entailment
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.TypeChecker.Entailment (
    entails
) where

import Data.Function (on)
import Data.List
import Data.Maybe (fromMaybe, maybeToList)
import Data.Foldable (foldMap)
import qualified Data.Map as M

import Control.Applicative
import Control.Arrow (Arrow(..))
import Control.Monad.State
import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Environment
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.TypeChecker.Unify
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types
import qualified Language.PureScript.Constants as C

newtype Work = Work Integer deriving (Show, Eq, Ord, Num)

-- |
-- Check that the current set of type class dictionaries entail the specified type class goal, and, if so,
-- return a type class dictionary reference.
--
entails :: Environment -> ModuleName -> M.Map (Maybe ModuleName) (M.Map (Qualified ProperName) (M.Map (Qualified Ident) TypeClassDictionaryInScope)) -> Constraint -> Bool -> Check Expr
entails env moduleName context = solve
  where
    forClassName :: Qualified ProperName -> [TypeClassDictionaryInScope]
    forClassName cn = findDicts cn Nothing ++ findDicts cn (Just moduleName)

    findDicts :: Qualified ProperName -> Maybe ModuleName -> [TypeClassDictionaryInScope]
    findDicts cn = maybe [] M.elems . (>>= M.lookup cn) . flip M.lookup context

    solve :: Constraint -> Bool -> Check Expr
    solve (className, tys) trySuperclasses = do
      let dicts = flip evalStateT (Work 0) $ go trySuperclasses className tys
      checkOverlaps dicts
      where
      go :: Bool -> Qualified ProperName -> [Type] -> StateT Work [] DictionaryValue
      go trySuperclasses' className' tys' = do
        workDone <- get
        guard $ workDone < 1000
        modify (1 +)
        directInstances <|> superclassInstances
        where
        directInstances :: StateT Work [] DictionaryValue
        directInstances = do
          tcd <- lift $ forClassName className'
          -- Make sure the type unifies with the type in the type instance definition
          subst <- lift . maybeToList . (>>= verifySubstitution) . fmap concat $ zipWithM (typeHeadsAreEqual moduleName env) tys' (tcdInstanceTypes tcd)
          -- Solve any necessary subgoals
          args <- solveSubgoals subst (tcdDependencies tcd)
          return $ mkDictionary (canonicalizeDictionary tcd) args

        superclassInstances :: StateT Work [] DictionaryValue
        superclassInstances = do
          guard trySuperclasses'
          (subclassName, (args, _, implies)) <- lift $ M.toList (typeClasses env)
          -- Try each superclass
          (index, (superclass, suTyArgs)) <- lift $ zip [0..] implies
          -- Make sure the type class name matches the superclass name
          guard $ className' == superclass
          -- Make sure the types unify with the types in the superclass implication
          subst <- lift . maybeToList . (>>= verifySubstitution) . fmap concat $ zipWithM (typeHeadsAreEqual moduleName env) tys' suTyArgs
          -- Finally, satisfy the subclass constraint
          args' <- lift . maybeToList $ mapM ((`lookup` subst) . fst) args
          suDict <- go True subclassName args'
          return $ SubclassDictionaryValue suDict superclass index

      -- Create dictionaries for subgoals which still need to be solved by calling go recursively
      -- E.g. the goal (Show a, Show b) => Show (Either a b) can be satisfied if the current type
      -- unifies with Either a b, and we can satisfy the subgoals Show a and Show b recursively.
      solveSubgoals :: [(String, Type)] -> Maybe [Constraint] -> StateT Work [] (Maybe [DictionaryValue])
      solveSubgoals _ Nothing = return Nothing
      solveSubgoals subst (Just subgoals) = do
        dict <- mapM (uncurry (go True) . second (map (replaceAllTypeVars subst))) subgoals
        return $ Just dict
        
      -- Make a dictionary from subgoal dictionaries by applying the correct function
      mkDictionary :: Qualified Ident -> Maybe [DictionaryValue] -> DictionaryValue
      mkDictionary fnName Nothing = LocalDictionaryValue fnName
      mkDictionary fnName (Just []) = GlobalDictionaryValue fnName
      mkDictionary fnName (Just dicts) = DependentDictionaryValue fnName dicts
      
      -- Turn a DictionaryValue into a Expr
      dictionaryValueToValue :: DictionaryValue -> Expr
      dictionaryValueToValue (LocalDictionaryValue fnName) = Var fnName
      dictionaryValueToValue (GlobalDictionaryValue fnName) = Var fnName
      dictionaryValueToValue (DependentDictionaryValue fnName dicts) = foldl App (Var fnName) (map dictionaryValueToValue dicts)
      dictionaryValueToValue (SubclassDictionaryValue dict superclassName index) =
        App (Accessor (C.__superclass_ ++ show superclassName ++ "_" ++ show index)
                      (dictionaryValueToValue dict))
            valUndefined
      -- Ensure that a substitution is valid
      verifySubstitution :: [(String, Type)] -> Maybe [(String, Type)]
      verifySubstitution subst = do
        let grps = groupBy ((==) `on` fst) subst
        guard (all (pairwise (unifiesWith env) . map snd) grps)
        return $ map head grps
      -- |
      -- Check for overlapping instances
      --
      checkOverlaps :: [DictionaryValue] -> Check Expr
      checkOverlaps dicts =
        case [ (d1, d2) | d1 <- dicts, d2 <- dicts, d1 `overlapping` d2 ] of
          ds@(_ : _) -> throwError . errorMessage $ OverlappingInstances className tys $ nub (map fst ds)
          _ -> case chooseSimplestDictionaries dicts of
                 [] -> throwError . errorMessage $ NoInstanceFound className tys
                 d : _ -> return $ dictionaryValueToValue d
      -- Choose the simplest DictionaryValues from a list of candidates
      -- The reason for this function is as follows:
      -- When considering overlapping instances, we don't want to consider the same dictionary
      -- to be an overlap of itself when obtained as a superclass of another class.
      -- Observing that we probably don't want to select a superclass instance when an instance
      -- is available directly, and that there is no way for a superclass instance to actually
      -- introduce an overlap that wouldn't have been there already, we simply remove dictionaries
      -- obtained as superclass instances if there are simpler instances available.
      chooseSimplestDictionaries :: [DictionaryValue] -> [DictionaryValue]
      chooseSimplestDictionaries ds = case filter isSimpleDictionaryValue ds of
                                        [] -> ds
                                        simple -> simple
      isSimpleDictionaryValue SubclassDictionaryValue{} = False
      isSimpleDictionaryValue (DependentDictionaryValue _ ds) = all isSimpleDictionaryValue ds
      isSimpleDictionaryValue _ = True
      -- |
      -- Check if two dictionaries are overlapping
      --
      -- Dictionaries which are subclass dictionaries cannot overlap, since otherwise the overlap would have
      -- been caught when constructing superclass dictionaries.
      --
      overlapping :: DictionaryValue -> DictionaryValue -> Bool
      overlapping (LocalDictionaryValue nm1)         (LocalDictionaryValue nm2)  | nm1 == nm2 = False
      overlapping (GlobalDictionaryValue nm1)        (GlobalDictionaryValue nm2) | nm1 == nm2 = False
      overlapping (DependentDictionaryValue nm1 ds1) (DependentDictionaryValue nm2 ds2)
        | nm1 == nm2 = or $ zipWith overlapping ds1 ds2
      overlapping SubclassDictionaryValue{} _ = False
      overlapping _ SubclassDictionaryValue{} = False
      overlapping _ _ = True

    valUndefined :: Expr
    valUndefined = Var (Qualified (Just (ModuleName [ProperName C.prim])) (Ident C.undefined))

-- |
-- Check whether the type heads of two types are equal (for the purposes of type class dictionary lookup),
-- and return a substitution from type variables to types which makes the type heads unify.
--
typeHeadsAreEqual :: ModuleName -> Environment -> Type -> Type -> Maybe [(String, Type)]
typeHeadsAreEqual _ _ (Skolem _ s1 _)      (Skolem _ s2 _)      | s1 == s2 = Just []
typeHeadsAreEqual _ _ t                    (TypeVar v)                     = Just [(v, t)]
typeHeadsAreEqual _ _ (TypeConstructor c1) (TypeConstructor c2) | c1 == c2 = Just []
typeHeadsAreEqual m e (TypeApp h1 t1)      (TypeApp h2 t2)                 = (++) <$> typeHeadsAreEqual m e h1 h2 
                                                                                  <*> typeHeadsAreEqual m e t1 t2
typeHeadsAreEqual m e (SaturatedTypeSynonym name args) t2 = case expandTypeSynonym' e name args of
  Left  _  -> Nothing
  Right t1 -> typeHeadsAreEqual m e t1 t2
typeHeadsAreEqual _ _ REmpty REmpty = Just []
typeHeadsAreEqual m e r1@(RCons _ _ _) r2@(RCons _ _ _) =
  let (s1, r1') = rowToList r1
      (s2, r2') = rowToList r2
      
      int = [ (t1, t2) | (name, t1) <- s1, (name', t2) <- s2, name == name' ]
      sd1 = [ (name, t1) | (name, t1) <- s1, name `notElem` map fst s2 ]
      sd2 = [ (name, t2) | (name, t2) <- s2, name `notElem` map fst s1 ]
  in (++) <$> foldMap (\(t1, t2) -> typeHeadsAreEqual m e t1 t2) int 
          <*> go sd1 r1' sd2 r2'
  where
  go :: [(String, Type)] -> Type -> [(String, Type)] -> Type -> Maybe [(String, Type)]
  go [] REmpty          [] REmpty          = Just [] 
  go [] (TUnknown _)    _  _               = Just [] 
  go [] (TypeVar v1)    [] (TypeVar v2)    | v1 == v2 = Just []
  go [] (Skolem _ s1 _) [] (Skolem _ s2 _) | s1 == s2 = Just []
  go sd r               [] (TypeVar v)     = Just [(v, rowFromList (sd, r))]
  go _  _               _  _               = Nothing
typeHeadsAreEqual _ _ _ _ = Nothing

-- |
-- Check all values in a list pairwise match a predicate
--
pairwise :: (a -> a -> Bool) -> [a] -> Bool
pairwise _ [] = True
pairwise _ [_] = True
pairwise p (x : xs) = all (p x) xs && pairwise p xs
