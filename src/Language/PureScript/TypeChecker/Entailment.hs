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

{-# LANGUAGE CPP #-}

module Language.PureScript.TypeChecker.Entailment (
    entails
) where

import Data.Function (on)
import Data.List
import Data.Maybe (maybeToList, mapMaybe)
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (foldMap)
#endif
import qualified Data.Map as M

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Arrow (Arrow(..))
import Control.Monad.State
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Writer.Class (tell)

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.TypeChecker.Unify
import Language.PureScript.TypeClassDictionaries
import Language.PureScript.Types
import qualified Language.PureScript.Constants as C

-- |
-- Check that the current set of type class dictionaries entail the specified type class goal, and, if so,
-- return a type class dictionary reference.
--
entails :: ModuleName -> M.Map (Maybe ModuleName) (M.Map (Qualified ProperName) (M.Map (Qualified Ident) TypeClassDictionaryInScope)) -> Constraint -> Check Expr
entails moduleName context = solve
  where
    forClassName :: Qualified ProperName -> [Type] -> [TypeClassDictionaryInScope]
    forClassName cn@(Qualified (Just mn) _) tys = concatMap (findDicts cn) (Nothing : Just mn : map Just (mapMaybe ctorModules tys))
    forClassName _ _ = internalError "forClassName: expected qualified class name"

    ctorModules :: Type -> Maybe ModuleName
    ctorModules (TypeConstructor (Qualified (Just mn) _)) = Just mn
    ctorModules (TypeConstructor (Qualified Nothing _)) = internalError "ctorModules: unqualified type name"
    ctorModules (TypeApp ty _) = ctorModules ty
    ctorModules _ = Nothing

    findDicts :: Qualified ProperName -> Maybe ModuleName -> [TypeClassDictionaryInScope]
    findDicts cn = maybe [] M.elems . (>>= M.lookup cn) . flip M.lookup context

    solve :: Constraint -> Check Expr
    solve (className, tys) = do
      dict <- go 0 className tys
      return $ dictionaryValueToValue dict
      where
      go :: Int -> Qualified ProperName -> [Type] -> Check DictionaryValue
      go work className' tys' | work > 1000 = throwError . errorMessage $ PossiblyInfiniteInstance className' tys'
      go work className' tys' = do
        let instances = do
              tcd <- forClassName className' tys'
              -- Make sure the type unifies with the type in the type instance definition
              subst <- maybeToList . (>>= verifySubstitution) . fmap concat $ zipWithM (typeHeadsAreEqual moduleName) tys' (tcdInstanceTypes tcd)
              return (subst, tcd)
        (subst, tcd) <- unique instances
        -- Solve any necessary subgoals
        args <- solveSubgoals subst (tcdDependencies tcd)
        return $ foldr (\(superclassName, index) dict -> SubclassDictionaryValue dict superclassName index)
                       (mkDictionary (tcdName tcd) args)
                       (tcdPath tcd)
        where

        unique :: [(a, TypeClassDictionaryInScope)] -> Check (a, TypeClassDictionaryInScope)
        unique [] = throwError . errorMessage $ NoInstanceFound className' tys'
        unique [a] = return a
        unique tcds | pairwise overlapping (map snd tcds) = do
                        tell . errorMessage $ OverlappingInstances className' tys' (map (tcdName . snd) tcds)
                        return (head tcds)
                    | otherwise = return (minimumBy (compare `on` length . tcdPath . snd) tcds)

        -- |
        -- Check if two dictionaries are overlapping
        --
        -- Dictionaries which are subclass dictionaries cannot overlap, since otherwise the overlap would have
        -- been caught when constructing superclass dictionaries.
        overlapping :: TypeClassDictionaryInScope -> TypeClassDictionaryInScope -> Bool
        overlapping TypeClassDictionaryInScope{ tcdPath = _ : _ } _ = False
        overlapping _ TypeClassDictionaryInScope{ tcdPath = _ : _ } = False
        overlapping TypeClassDictionaryInScope{ tcdDependencies = Nothing } _ = False
        overlapping _ TypeClassDictionaryInScope{ tcdDependencies = Nothing } = False
        overlapping tcd1 tcd2 = tcdName tcd1 /= tcdName tcd2

        -- Create dictionaries for subgoals which still need to be solved by calling go recursively
        -- E.g. the goal (Show a, Show b) => Show (Either a b) can be satisfied if the current type
        -- unifies with Either a b, and we can satisfy the subgoals Show a and Show b recursively.
        solveSubgoals :: [(String, Type)] -> Maybe [Constraint] -> Check (Maybe [DictionaryValue])
        solveSubgoals _ Nothing = return Nothing
        solveSubgoals subst (Just subgoals) = do
          dict <- mapM (uncurry (go (work + 1)) . second (map (replaceAllTypeVars subst))) subgoals
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
        App (Accessor (C.__superclass_ ++ showQualified runProperName superclassName ++ "_" ++ show index)
                      (dictionaryValueToValue dict))
            valUndefined
      -- Ensure that a substitution is valid
      verifySubstitution :: [(String, Type)] -> Maybe [(String, Type)]
      verifySubstitution subst = do
        let grps = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ subst
        guard (all (pairwise unifiesWith . map snd) grps)
        return $ map head grps

    valUndefined :: Expr
    valUndefined = Var (Qualified (Just (ModuleName [ProperName C.prim])) (Ident C.undefined))

-- |
-- Check whether the type heads of two types are equal (for the purposes of type class dictionary lookup),
-- and return a substitution from type variables to types which makes the type heads unify.
--
typeHeadsAreEqual :: ModuleName -> Type -> Type -> Maybe [(String, Type)]
typeHeadsAreEqual _ (Skolem _ s1 _)      (Skolem _ s2 _)      | s1 == s2 = Just []
typeHeadsAreEqual _ t                    (TypeVar v)                     = Just [(v, t)]
typeHeadsAreEqual _ (TypeConstructor c1) (TypeConstructor c2) | c1 == c2 = Just []
typeHeadsAreEqual m (TypeApp h1 t1)      (TypeApp h2 t2)                 = (++) <$> typeHeadsAreEqual m h1 h2
                                                                                <*> typeHeadsAreEqual m t1 t2
typeHeadsAreEqual _ REmpty REmpty = Just []
typeHeadsAreEqual m r1@RCons{} r2@RCons{} =
  let (s1, r1') = rowToList r1
      (s2, r2') = rowToList r2

      int = [ (t1, t2) | (name, t1) <- s1, (name', t2) <- s2, name == name' ]
      sd1 = [ (name, t1) | (name, t1) <- s1, name `notElem` map fst s2 ]
      sd2 = [ (name, t2) | (name, t2) <- s2, name `notElem` map fst s1 ]
  in (++) <$> foldMap (uncurry (typeHeadsAreEqual m)) int
          <*> go sd1 r1' sd2 r2'
  where
  go :: [(String, Type)] -> Type -> [(String, Type)] -> Type -> Maybe [(String, Type)]
  go [] REmpty          [] REmpty          = Just []
  go [] (TUnknown _)    _  _               = Just []
  go [] (TypeVar v1)    [] (TypeVar v2)    | v1 == v2 = Just []
  go [] (Skolem _ s1 _) [] (Skolem _ s2 _) | s1 == s2 = Just []
  go sd r               [] (TypeVar v)     = Just [(v, rowFromList (sd, r))]
  go _  _               _  _               = Nothing
typeHeadsAreEqual _ _ _ = Nothing

-- |
-- Check all values in a list pairwise match a predicate
--
pairwise :: (a -> a -> Bool) -> [a] -> Bool
pairwise _ [] = True
pairwise _ [_] = True
pairwise p (x : xs) = all (p x) xs && pairwise p xs
