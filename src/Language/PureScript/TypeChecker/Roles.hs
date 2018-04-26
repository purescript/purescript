-- |
-- Role inference
--
module Language.PureScript.TypeChecker.Roles
  ( inferRoles
  ) where

import Prelude.Compat

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)

import Language.PureScript.Environment
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Roles
import Language.PureScript.Types

-- |
-- Given an environment and the qualified name of a type constructor in that
-- environment, returns a list of (formal parameter name, role) pairs, in the
-- order they are defined in the type definition.
inferRoles :: Environment -> Qualified (ProperName 'TypeName) -> [(Text, Role)]
inferRoles env tyName
  | Just roles <- lookup tyName primRoles =
      -- A built-in type constructor for which we have an explicit role
      -- signature.
      roles
  | Just roles <- M.lookup tyName (roleDeclarations env) =
      rolesFromDeclaration roles
  | Just (_, DataType tvs ctors) <- envMeta =
      -- A plain data type. For each constructor the type has, walk its list of
      -- field types and accumulate a list of (formal parameter name, role)
      -- pairs. Then, walk the list of defined parameters, ensuring both that
      -- every parameter appears (with a default role of phantom) and that they
      -- appear in the right order.
      let ctorRoles = foldMap (foldMap walk . snd) ctors
      in  map (\(tv, _) -> (tv, fromMaybe Phantom (lookup tv ctorRoles))) tvs
  | Just (k, ExternData) <- envMeta =
      -- A foreign data type. Since the type will have no defined constructors
      -- nor associated data types, infer the set of type parameters from its
      -- kind and assume in the absence of role signatures that all such
      -- parameters are representational.
      rolesFromForeignTypeKind k
  | otherwise =
      []
  where
    envTypes = types env
    envMeta = M.lookup tyName envTypes
    -- This function is named walk to match the specification given in the "Role
    -- inference" section of the paper "Safe Zero-cost Coercions for Haskell".
    walk (TypeVar _ v) =
      -- A type variable standing alone (e.g. `a` in `data D a b = D a`) is
      -- representational.
      [(v, Representational)]
    walk (ForAll _ _ t _) =
      -- We can walk under universal quantifiers. Note that this might appear
      -- erroneous -- given a definition like `data D a = D (forall r. r -> a)`
      -- we expect we'll produce `[("r", Representational)]` as part of the
      -- recursive call's result. This is true, but fine, since this tuple will
      -- never be looked up when building the final result, which only looks at
      -- the variables defined as parameters to the type.
      walk t
    walk t
      | Just (t1, t2s) <- splitTypeApp t =
          case t1 of
            -- If the type is an application of a type constructor to some
            -- arguments, recursively infer the roles of the type constructor's
            -- arguments. For each (role, argument) pair, recurse if the
            -- argument is representational (since its use of our parameters is
            -- important) and terminate if the argument is phantom.
            TypeConstructor _ t1Name ->
              let t1Roles = inferRoles env t1Name
                  k (_v, role) ti = case role of
                    Representational ->
                      walk ti
                    Phantom ->
                      []
              in  concat (zipWith k t1Roles t2s)
            -- If the type is an application of any other type-level term, walk
            -- both the first and argument types to determine what role
            -- contributions they make (e.g. in the case
            -- `data F a b = F (a (G b))`, the use of `a` as a
            -- function/constructor would trigger this case and both `a` and
            -- `G b` would be walked recursively).
            _ ->
              walk t1 ++ foldMap walk t2s
      | otherwise =
          []

rolesFromDeclaration :: [Role] -> [(Text, Role)]
rolesFromDeclaration
  = zipWith mkPair [0..]
  where
    mkPair :: Int -> Role -> (Text, Role)
    mkPair i r =
      (pack ("a" ++ show i), r)

-- |
-- Given the kind of a foreign type, generate a list of formal parameter names
-- each tied to a `Representational` role which, in the absence of role
-- signatures, provides the safest role signature which can be assigned to a
-- type whose constructors are opaque to us.
rolesFromForeignTypeKind :: SourceKind -> [(Text, Role)]
rolesFromForeignTypeKind
  = zipWith mkPair [0..] . go []
  where
    mkPair :: Int -> SourceKind -> (Text, Role)
    mkPair i _k =
      (pack ("a" ++ show i), Representational)
    go acc = \case
      FunKind _ k1 k2 ->
        go (k2 : acc) k1
      k ->
        k : acc

-- |
-- A lookup table of role definitions for primitive types whose constructors
-- won't be present in any environment.
primRoles :: [(Qualified (ProperName 'TypeName), [(Text, Role)])]
primRoles
  = [ (primName "Function", [("a", Representational), ("b", Representational)])
    , (primName "Array", [("a", Representational)])
    , (primName "Record", [("r", Representational)])
    ]
