{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Role inference
--
module Language.PureScript.TypeChecker.Roles
  ( lookupRoles
  , checkRoles
  , checkDataBindingGroupRoles
  ) where

import Prelude.Compat

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State (MonadState(..), runState, state)
import Data.Coerce (coerce)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Semigroup (Any(..))
import Data.Text (Text)

import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Roles
import Language.PureScript.Types

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

type RoleEnv = M.Map (Qualified (ProperName 'TypeName)) [Role]

typeKindRoles :: TypeKind -> Maybe [Role]
typeKindRoles = \case
  DataType _ args _ ->
    Just $ map (\(_, _, role) -> role) args
  ExternData roles ->
    Just roles
  _ ->
    Nothing

getRoleEnv :: Environment -> RoleEnv
getRoleEnv env =
  M.mapMaybe (typeKindRoles . snd) (types env)

updateRoleEnv
  :: Qualified (ProperName 'TypeName)
  -> [Role]
  -> RoleEnv
  -> (Any, RoleEnv)
updateRoleEnv qualTyName roles' roleEnv =
  let roles = fromMaybe (repeat Phantom) $ M.lookup qualTyName roleEnv
      mostRestrictiveRoles = zipWith min roles roles'
      didRolesChange = any (uncurry (<)) $ zip mostRestrictiveRoles roles
  in (Any didRolesChange, M.insert qualTyName mostRestrictiveRoles roleEnv)

-- |
-- Lookup the roles for a type in the environment. If the type does not have
-- roles (e.g. is a type synonym or a type variable), then this function
-- returns an empty list.
--
lookupRoles
  :: Environment
  -> Qualified (ProperName 'TypeName)
  -> [Role]
lookupRoles env tyName =
  fromMaybe [] $ M.lookup tyName (types env) >>= typeKindRoles . snd

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
   . (MonadError MultipleErrors m)
  => Environment
  -> ModuleName
  -> ProperName 'TypeName
    -- ^ The name of the data type whose roles we are checking
  -> [(Text, Maybe SourceType)]
    -- ^ type parameters for the data type whose roles we are checking
  -> [DataConstructorDeclaration]
    -- ^ constructors of the data type whose roles we are checking
  -> m [Role]
checkRoles env moduleName tyName tyArgs ctors =
  checkDataBindingGroupRoles env moduleName [(tyName, tyArgs, ctors)] tyName tyArgs

type DataDeclaration =
  ( ProperName 'TypeName
  , [(Text, Maybe SourceType)]
  , [DataConstructorDeclaration]
  )

checkDataBindingGroupRoles
  :: forall m
   . (MonadError MultipleErrors m)
  => Environment
  -> ModuleName
  -> [DataDeclaration]
  -> ProperName 'TypeName
  -> [(Text, Maybe SourceType)]
  -> m [Role]
checkDataBindingGroupRoles env moduleName group =
  let initialRoleEnv = M.union (roleDeclarations env) (getRoleEnv env)
      inferredRoleEnv = inferDataBindingGroupRoles moduleName group initialRoleEnv
  in \tyName tyArgs -> do
    let qualTyName = Qualified (Just moduleName) tyName
        inferredRoles = M.lookup qualTyName inferredRoleEnv
    rethrow (addHint (ErrorInRoleDeclaration tyName)) $ do
      case M.lookup qualTyName (roleDeclarations env) of
        Just declaredRoles -> do
          let
            k (var, _) inf dec =
              if inf < dec
                then throwError . errorMessage $ RoleMismatch var inf dec
                else pure dec
          sequence $ zipWith3 k tyArgs (fromMaybe (repeat Phantom) inferredRoles) declaredRoles
        Nothing ->
          pure $ fromMaybe (Phantom <$ tyArgs) inferredRoles

inferDataBindingGroupRoles
  :: ModuleName
  -> [DataDeclaration]
  -> RoleEnv
  -> RoleEnv
inferDataBindingGroupRoles moduleName group roleEnv =
  let (Any didRolesChange, roleEnv') = flip runState roleEnv $
        mconcat <$> traverse (state . inferDataDeclarationRoles moduleName) group
  in if didRolesChange
     then inferDataBindingGroupRoles moduleName group roleEnv'
     else roleEnv'

-- |
-- Infers roles for the given data type declaration, along with a flag to tell
-- if more restrictive roles were added to the environment.
--
inferDataDeclarationRoles
  :: ModuleName
  -> DataDeclaration
  -> RoleEnv
  -> (Any, RoleEnv)
inferDataDeclarationRoles moduleName (tyName, tyArgs, ctors) roleEnv =
  let qualTyName = Qualified (Just moduleName) tyName
      ctorRoles = getRoleMap . foldMap (walk mempty . snd) $ ctors >>= dataCtorFields
      inferredRoles = map (\(arg, _) -> fromMaybe Phantom (M.lookup arg ctorRoles)) tyArgs
  in updateRoleEnv qualTyName inferredRoles roleEnv
  where
  -- This function is named @walk@ to match the specification given in the
  -- "Role inference" section of the paper "Safe Zero-cost Coercions for
  -- Haskell".
  walk :: S.Set Text -> SourceType -> RoleMap
  walk btvs (TypeVar _ v)
    -- A type variable standing alone (e.g. @a@ in @data D a b = D a@) is
    -- representational, _unless_ it has been bound by a quantifier, in which
    -- case it is not actually a parameter to the type (e.g. @z@ in
    -- @data T z = T (forall z. z -> z)@).
    | S.member v btvs =
        mempty
    | otherwise =
        RoleMap $ M.singleton v Representational
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
  walk btvs (ConstrainedType _ Constraint{..} t) =
    -- For constrained types, mark all free variables in the constraint
    -- arguments as nominal and recurse on the type beneath the constraint.
    walk btvs t <> foldMap (freeNominals btvs) constraintArgs
  walk btvs (RCons _ _ thead ttail) = do
    -- For row types, we just walk along them and collect the results.
    walk btvs thead <> walk btvs ttail
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
          TypeConstructor _ t1Name ->
            let
              t1Roles = fromMaybe (repeat Phantom) $ M.lookup t1Name roleEnv
              k role ti = case role of
                Nominal ->
                  freeNominals btvs ti
                Representational ->
                  go ti
                Phantom ->
                  mempty
            in mconcat (zipWith k t1Roles t2s)
          -- If the type is an application of any other type-level term, walk
          -- that term to collect its roles and mark all free variables in
          -- its argument as nominal.
          _ -> do
            go t1 <> foldMap (freeNominals btvs) t2s
    | otherwise =
        mempty
    where
      go = walk btvs

-- Given a type, computes the list of free variables in that type
-- (taking into account those bound in @walk@) and returns a @RoleMap@
-- ascribing a nominal role to each of those variables.
freeNominals :: S.Set Text -> SourceType -> RoleMap
freeNominals btvs x =
  let ftvs = filter (flip S.notMember btvs) (freeTypeVariables x)
  in  RoleMap (M.fromList $ map (, Nominal) ftvs)
