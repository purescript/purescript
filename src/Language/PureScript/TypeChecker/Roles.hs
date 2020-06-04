{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Role inference
--
module Language.PureScript.TypeChecker.Roles
  ( inferRoles
  ) where

import Prelude.Compat

import Data.Coerce (coerce)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)

import Language.PureScript.Environment
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

-- |
-- Given an environment and the qualified name of a type constructor in that
-- environment, returns a list of roles, in the order they are defined in the
-- type definition.
inferRoles :: Environment -> Qualified (ProperName 'TypeName) -> [Role]
inferRoles env tyName
  | Just roles <- M.lookup tyName (roleDeclarations env) =
      roles
  | Just (_, DataType tvs ctors) <- envMeta =
      -- A plain data type. For each constructor the type has, walk its list of
      -- field types and accumulate a list of (formal parameter name, role)
      -- pairs. Then, walk the list of defined parameters, ensuring both that
      -- every parameter appears (with a default role of phantom) and that they
      -- appear in the right order.
      let ctorRoles = getRoleMap $ foldMap (foldMap (walk mempty) . snd) ctors
      in  map (\(tv, _) -> fromMaybe Phantom (M.lookup tv ctorRoles)) tvs
  | Just (k, ExternData) <- envMeta =
      -- A foreign data type. Since the type will have no defined constructors
      -- nor associated data types, infer the set of type parameters from its
      -- kind and assume in the absence of role signatures that all such
      -- parameters are nominal.
      rolesFromForeignTypeKind k
  | otherwise =
      []
  where
    envTypes = types env
    envMeta = M.lookup tyName envTypes
    -- This function is named walk to match the specification given in the "Role
    -- inference" section of the paper "Safe Zero-cost Coercions for Haskell".
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
    walk btvs (RCons _ _ thead ttail) =
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
              let t1Roles = inferRoles env t1Name
                  k role ti = case role of
                    Nominal ->
                      freeNominals ti
                    Representational ->
                      go ti
                    Phantom ->
                      mempty
              in  mconcat (zipWith k t1Roles t2s)
            -- If the type is an application of any other type-level term, walk
            -- that term to collect its roles and mark all free variables in
            -- its argument as nominal.
            _ ->
              go t1 <> foldMap freeNominals t2s
      | otherwise =
          mempty
      where
        go = walk btvs
        -- Given a type, computes the list of free variables in that type
        -- (taking into account those bound in @walk@) and returns a @RoleMap@
        -- ascribing a nominal role to each of those variables.
        freeNominals x =
          let ftvs = filter (flip S.notMember btvs) (freeTypeVariables x)
          in  RoleMap (M.fromList $ map (, Nominal) ftvs)

-- |
-- Given the kind of a foreign type, generate a list @Nominal@ roles which, in
-- the absence of a role signature, provides the safest default for a type whose
-- constructors are opaque to us.
rolesFromForeignTypeKind :: SourceType -> [Role]
rolesFromForeignTypeKind k = replicate (kindArity k) Nominal

kindArity :: SourceType -> Int
kindArity = go 0 where
  go n (TypeApp _ (TypeApp _ fn _) k)
    | fn == tyFunction = go (n + 1) k
  go n (ForAll _ _ _ k _) = go n k
  go n _ = n
