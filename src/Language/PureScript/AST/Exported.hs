module Language.PureScript.AST.Exported
  ( exportedDeclarations
  , isExported
  ) where

import Prelude.Compat

import Control.Category ((>>>))

import Data.Maybe (mapMaybe)

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
exportedDeclarations :: Module -> [Declaration]
exportedDeclarations (Module _ _ mn decls exps) = go decls
  where
  go = flattenDecls
        >>> filter (isExported exps)
        >>> map (filterDataConstructors exps)
        >>> filterInstances mn exps

-- |
-- Filter out all data constructors from a declaration which are not exported.
-- If the supplied declaration is not a data declaration, this function returns
-- it unchanged.
--
filterDataConstructors :: Maybe [DeclarationRef] -> Declaration -> Declaration
filterDataConstructors exps (DataDeclaration dType tyName tyArgs dctors) =
  DataDeclaration dType tyName tyArgs $
    filter (isDctorExported tyName exps . fst) dctors
filterDataConstructors exps (PositionedDeclaration srcSpan coms d) =
  PositionedDeclaration srcSpan coms (filterDataConstructors exps d)
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
typeInstanceConstituents (TypeInstanceDeclaration _ constraints className tys _) =
  Left className : (concatMap fromConstraint constraints ++ concatMap fromType tys)
  where

  fromConstraint c = Left (constraintClass c) : concatMap fromType (constraintArgs c)
  fromType = everythingOnTypes (++) go

  -- Note that type synonyms are disallowed in instance declarations, so
  -- we don't need to handle them here.
  go (TypeConstructor n) = [Right n]
  go (ConstrainedType c _) = fromConstraint c
  go _ = []

typeInstanceConstituents (PositionedDeclaration _ _ d) = typeInstanceConstituents d
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
isExported exps (PositionedDeclaration _ _ d) = isExported exps d
isExported (Just exps) decl = any (matches decl) exps
  where
  matches (TypeDeclaration ident _) (ValueRef _ ident') = ident == ident'
  matches (ValueDeclaration ident _ _ _) (ValueRef _ ident') = ident == ident'
  matches (ExternDeclaration ident _) (ValueRef _ ident') = ident == ident'
  matches (DataDeclaration _ ident _ _) (TypeRef _ ident' _) = ident == ident'
  matches (ExternDataDeclaration ident _) (TypeRef _ ident' _) = ident == ident'
  matches (ExternKindDeclaration ident) (KindRef _ ident') = ident == ident'
  matches (TypeSynonymDeclaration ident _ _) (TypeRef _ ident' _) = ident == ident'
  matches (TypeClassDeclaration ident _ _ _ _) (TypeClassRef _ ident') = ident == ident'
  matches (ValueFixityDeclaration _ _ op) (ValueOpRef _ op') = op == op'
  matches (TypeFixityDeclaration _ _ op) (TypeOpRef _ op') = op == op'
  matches (PositionedDeclaration _ _ d) r = d `matches` r
  matches _ _ = False

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
