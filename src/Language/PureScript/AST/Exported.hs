module Language.PureScript.AST.Exported
  ( exportedDeclarations
  , isExported
  ) where

import Prelude.Compat
import Protolude (sortBy, on)

import Control.Category ((>>>))

import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Language.PureScript.AST.Declarations
import Language.PureScript.Types
import Language.PureScript.Names

-- |
-- Return a list of all declarations which are exported from a module.
-- This function descends into data declarations to filter out unexported
-- data constructors, and also filters out type instance declarations if
-- they refer to classes or types which are not themselves exported.
--
-- Note that this function assumes that the module has already had its imports
-- desugared using 'Language.PureScript.Sugar.Names.desugarImports'. It will
-- produce incorrect results if this is not the case - for example, type class
-- instances will be incorrectly removed in some cases.
--
-- The returned declarations are in the same order as they appear in the export
-- list, unless there is no export list, in which case they appear in the same
-- order as they do in the source file.
--
exportedDeclarations :: Module -> [Declaration]
exportedDeclarations (Module _ _ mn decls exps) = go decls
  where
  go = flattenDecls
        >>> filter (isExported exps)
        >>> map (filterDataConstructors exps)
        >>> filterInstances mn exps
        >>> maybe id reorder exps

-- |
-- Filter out all data constructors from a declaration which are not exported.
-- If the supplied declaration is not a data declaration, this function returns
-- it unchanged.
--
filterDataConstructors :: Maybe [DeclarationRef] -> Declaration -> Declaration
filterDataConstructors exps (DataDeclaration sa dType tyName tyArgs dctors) =
  DataDeclaration sa dType tyName tyArgs $
    filter (isDctorExported tyName exps . fst) dctors
filterDataConstructors _ other = other

-- |
-- Filter out all the type instances from a list of declarations which
-- reference a type or type class which is both local and not exported.
--
-- Note that this function assumes that the module has already had its imports
-- desugared using "Language.PureScript.Sugar.Names.desugarImports". It will
-- produce incorrect results if this is not the case - for example, type class
-- instances will be incorrectly removed in some cases.
--
filterInstances
  :: ModuleName
  -> Maybe [DeclarationRef]
  -> [Declaration]
  -> [Declaration]
filterInstances _ Nothing = id
filterInstances mn (Just exps) =
  let refs = Left `map` mapMaybe typeClassName exps
          ++ Right `map` mapMaybe typeName exps
  in filter (all (visibleOutside refs) . typeInstanceConstituents)
  where
  -- Given a Qualified ProperName, and a list of all exported types and type
  -- classes, returns whether the supplied Qualified ProperName is visible
  -- outside this module. This is true if one of the following hold:
  --
  --  * the name is defined in the same module and is exported,
  --  * the name is defined in a different module (and must be exported from
  --    that module; the code would fail to compile otherwise).
  visibleOutside
    :: [Either (ProperName 'ClassName) (ProperName 'TypeName)]
    -> Either (Qualified (ProperName 'ClassName)) (Qualified (ProperName 'TypeName))
    -> Bool
  visibleOutside refs q
    | either checkQual checkQual q = True
    | otherwise = either (Left . disqualify) (Right . disqualify) q `elem` refs

  -- Check that a qualified name is qualified for a different module
  checkQual :: Qualified a -> Bool
  checkQual q = isQualified q && not (isQualifiedWith mn q)

  typeName :: DeclarationRef -> Maybe (ProperName 'TypeName)
  typeName (TypeRef _ n _) = Just n
  typeName _ = Nothing

  typeClassName :: DeclarationRef -> Maybe (ProperName 'ClassName)
  typeClassName (TypeClassRef _ n) = Just n
  typeClassName _ = Nothing

-- |
-- Get all type and type class names referenced by a type instance declaration.
--
typeInstanceConstituents :: Declaration -> [Either (Qualified (ProperName 'ClassName)) (Qualified (ProperName 'TypeName))]
typeInstanceConstituents (TypeInstanceDeclaration _ _ _ _ constraints className tys _) =
  Left className : (concatMap fromConstraint constraints ++ concatMap fromType tys)
  where

  fromConstraint c = Left (constraintClass c) : concatMap fromType (constraintArgs c)
  fromType = everythingOnTypes (++) go

  -- Note that type synonyms are disallowed in instance declarations, so
  -- we don't need to handle them here.
  go (TypeConstructor n) = [Right n]
  go (ConstrainedType c _) = fromConstraint c
  go _ = []

typeInstanceConstituents _ = []


-- |
-- Test if a declaration is exported, given a module's export list. Note that
-- this function does not account for type instance declarations of
-- non-exported types, or non-exported data constructors. Therefore, you should
-- prefer 'exportedDeclarations' to this function, where possible.
--
isExported :: Maybe [DeclarationRef] -> Declaration -> Bool
isExported Nothing _ = True
isExported _ TypeInstanceDeclaration{} = True
isExported (Just exps) decl = any matches exps
  where
  matches declRef = declName decl == Just (declRefName declRef)

-- |
-- Test if a data constructor for a given type is exported, given a module's
-- export list. Prefer 'exportedDeclarations' to this function, where possible.
--
isDctorExported :: ProperName 'TypeName -> Maybe [DeclarationRef] -> ProperName 'ConstructorName -> Bool
isDctorExported _ Nothing _ = True
isDctorExported ident (Just exps) ctor = test `any` exps
  where
  test (TypeRef _ ident' Nothing) = ident == ident'
  test (TypeRef _ ident' (Just ctors)) = ident == ident' && ctor `elem` ctors
  test _ = False

-- |
-- Reorder declarations based on the order they appear in the given export
-- list.
--
reorder :: [DeclarationRef] -> [Declaration] -> [Declaration]
reorder refs =
  sortBy (compare `on` refIndex)
  where
  refIndices =
    M.fromList $ zip (map declRefName refs) [(0::Int)..]
  refIndex decl =
    declName decl >>= flip M.lookup refIndices
