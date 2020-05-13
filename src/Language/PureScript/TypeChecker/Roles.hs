{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Role inference
--
module Language.PureScript.TypeChecker.Roles
  ( lookupEnvRoles
  , checkRoles
  ) where

import Prelude.Compat

import Control.Monad
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..))
import Data.Coerce (coerce)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)

import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Roles
import Language.PureScript.Types
import Language.PureScript.TypeChecker.Monad

-- |
-- A map of a type's formal parameter names to their roles. This type's
-- @Semigroup@ and @Monoid@ instances preserve the least-permissive role
-- ascribed to any given variable, as defined by the @Role@ type's @Ord@
-- instance. That is, a variable that has been marked as @Nominal@ can not
-- later be marked @Representational@, and so on.
newtype RoleMap = RoleMap { getRoleMap :: M.Map Text Role }

instance Semigroup RoleMap where
  (<>) =
    coerce @(M.Map Text Role -> _ -> _) @(RoleMap -> _ -> _) (M.unionWith min)

instance Monoid RoleMap where
  mempty =
    RoleMap M.empty

-- |
-- Lookup the roles for a type in the environment. If the type does not have
-- roles (e.g. is a type synonym or a type variable), then this function
-- returns an empty list.
--
lookupEnvRoles
  :: Environment
  -> Qualified (ProperName 'TypeName)
  -> [Role]
lookupEnvRoles env tyName =
  case fmap snd $ M.lookup tyName (types env) of
    Just (DataType args _) ->
      map (\(_, _, role) -> role) args
    Just (ExternData roles) ->
      roles
    _ ->
      []

-- | This function does the following:
--
-- * Infers roles for the given data type declaration
--
-- * Compares the inferred roles to the explicitly declared roles (if any) and
--   ensures that the explicitly declared roles are not more permissive than
--   the inferred ones
--
checkRoles
  :: forall m
   . (MonadError MultipleErrors m, MonadState CheckState m)
  => ModuleName
  -> ProperName 'TypeName
    -- ^ The name of the data type whose roles we are checking
  -> [(Text, Maybe SourceType)]
    -- ^ type parameters for the data type whose roles we are checking
  -> [DataConstructorDeclaration]
    -- ^ constructors of the data type whose roles we are checking
  -> m [Role]
checkRoles moduleName tyName tyArgs ctors = do
  let qualName = Qualified (Just moduleName) tyName
  roleMaps <- traverse (walk mempty . snd) $ ctors >>= dataCtorFields
  let
    ctorRoles = getRoleMap $ mconcat roleMaps
    inferredRoles = map (\(arg, _) -> fromMaybe Phantom (M.lookup arg ctorRoles)) tyArgs
  env <- getEnv
  rethrow (addHint (ErrorInRoleDeclaration tyName)) $ do
    case M.lookup qualName (roleDeclarations env) of
      Just declaredRoles -> do
        let
          k (var, _) inf dec =
            if inf < dec
              then throwError . errorMessage $ RoleMismatch var inf dec
              else pure dec
        sequence $ zipWith3 k tyArgs inferredRoles declaredRoles
      Nothing ->
        pure inferredRoles
  where
  -- This function is named @walk@ to match the specification given in the
  -- "Role inference" section of the paper "Safe Zero-cost Coercions for
  -- Haskell".
  walk :: S.Set Text -> SourceType -> m RoleMap
  walk btvs (TypeVar _ v)
    -- A type variable standing alone (e.g. @a@ in @data D a b = D a@) is
    -- representational, _unless_ it has been bound by a quantifier, in which
    -- case it is not actually a parameter to the type (e.g. @z@ in
    -- @data T z = T (forall z. z -> z)@).
    | S.member v btvs =
        pure mempty
    | otherwise =
        pure $ RoleMap $ M.singleton v Representational
  walk btvs (ForAll _ tv _ t _) =
    -- We can walk under universal quantifiers as long as we make note of the
    -- variables that they bind. For instance, given a definition
    -- @data T z = T (forall z. z -> z)@, we will make note that @z@ is bound
    -- by a quantifier so that we do not mark @T@'s parameter as
    -- representational later on. Similarly, given a definition like
    -- @data D a = D (forall r. r -> a)@, we'll mark @r@ as bound so that it
    -- doesn't appear as a spurious parameter to @D@ when we complete
    -- inference.
    walk (S.insert tv btvs) t
  walk btvs (RCons _ _ thead ttail) = do
    -- For row types, we just walk along them and collect the results.
    h <- walk btvs thead
    t <- walk btvs ttail
    pure (h <> t)
  walk btvs (KindedType _ t _k) =
    -- For kind-annotated types, discard the annotation and recurse on the
    -- type beneath.
    walk btvs t
  walk btvs t
    | (t1, _, t2s) <- unapplyTypes t
    , not $ null t2s =
        case t1 of
          -- If the type is an application of a type constructor to some
          -- arguments, recursively infer the roles of the type constructor's
          -- arguments. For each (role, argument) pair:
          --
          -- * If the role is nominal, mark all free variables in the
          --   argument as nominal also, since they cannot be coerced if the
          --   argument's nominality is to be preserved.
          -- * If the role is representational, recurse on the argument, since
          --   its use of our parameters is important.
          -- * If the role is phantom, terminate, since the argument's use of
          --   our parameters is unimportant.
          TypeConstructor _ t1Name -> do
            env <- getEnv
            let
              t1Roles = lookupEnvRoles env t1Name
              k role ti = case role of
                Nominal ->
                  pure $ freeNominals ti
                Representational ->
                  go ti
                Phantom ->
                  pure mempty
            fmap mconcat (zipWithM k t1Roles t2s)
          -- If the type is an application of any other type-level term, walk
          -- that term to collect its roles and mark all free variables in
          -- its argument as nominal.
          _ -> do
            r <- go t1
            pure (r <> foldMap freeNominals t2s)
    | otherwise =
        pure mempty
    where
      go = walk btvs
      -- Given a type, computes the list of free variables in that type
      -- (taking into account those bound in @walk@) and returns a @RoleMap@
      -- ascribing a nominal role to each of those variables.
      freeNominals x =
        let ftvs = filter (flip S.notMember btvs) (freeTypeVariables x)
        in  RoleMap (M.fromList $ map (, Nominal) ftvs)
