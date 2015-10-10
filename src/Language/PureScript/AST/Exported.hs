
module Language.PureScript.AST.Exported (
  exportedDeclarations,
  isExported
) where

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
exportedDeclarations (Module _ _ _ decls exps) = go decls
  where
  go = flattenDecls
        >>> filter (isExported exps)
        >>> map (filterDataConstructors exps)
        >>> filterInstances exps

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
filterInstances :: Maybe [DeclarationRef] -> [Declaration] -> [Declaration]
filterInstances Nothing = id
filterInstances (Just exps) =
  let refs = mapMaybe typeName exps ++ mapMaybe typeClassName exps
  in filter (all (visibleOutside refs) . typeInstanceConstituents)
  where
  -- Given a Qualified ProperName, and a list of all exported types and type
  -- classes, returns whether the supplied Qualified ProperName is visible
  -- outside this module. This is true if one of the following hold:
  --
  --  * the name is defined in the same module and is exported,
  --  * the name is defined in a different module (and must be exported from
  --    that module; the code would fail to compile otherwise).
  visibleOutside _ (Qualified (Just _) _) = True
  visibleOutside refs (Qualified Nothing n) = n `elem` refs

  typeName (TypeRef n _) = Just n
  typeName (PositionedDeclarationRef _ _ r) = typeName r
  typeName _ = Nothing

  typeClassName (TypeClassRef n) = Just n
  typeClassName (PositionedDeclarationRef _ _ r) = typeClassName r
  typeClassName _ = Nothing

-- |
-- Get all type and type class names referenced by a type instance declaration.
--
typeInstanceConstituents :: Declaration -> [Qualified ProperName]
typeInstanceConstituents (TypeInstanceDeclaration _ constraints className tys _) =
  className : (concatMap fromConstraint constraints ++ concatMap fromType tys)
  where

  fromConstraint (name, tys') = name : concatMap fromType tys'
  fromType = everythingOnTypes (++) go

  -- Note that type synonyms are disallowed in instance declarations, so
  -- we don't need to handle them here.
  go (TypeConstructor n) = [n]
  go (ConstrainedType cs _) = concatMap fromConstraint cs
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
  matches (TypeDeclaration ident _)          (ValueRef ident')     = ident == ident'
  matches (ValueDeclaration ident _ _ _)     (ValueRef ident')     = ident == ident'
  matches (ExternDeclaration ident _)        (ValueRef ident')     = ident == ident'
  matches (FixityDeclaration _ name)         (ValueRef ident')     = name == runIdent ident'
  matches (DataDeclaration _ ident _ _)      (TypeRef ident' _)    = ident == ident'
  matches (ExternDataDeclaration ident _)    (TypeRef ident' _)    = ident == ident'
  matches (TypeSynonymDeclaration ident _ _) (TypeRef ident' _)    = ident == ident'
  matches (TypeClassDeclaration ident _ _ _) (TypeClassRef ident') = ident == ident'

  matches (PositionedDeclaration _ _ d) r = d `matches` r
  matches d (PositionedDeclarationRef _ _ r) = d `matches` r
  matches _ _ = False

-- |
-- Test if a data constructor for a given type is exported, given a module's
-- export list. Prefer 'exportedDeclarations' to this function, where possible.
--
isDctorExported :: ProperName -> Maybe [DeclarationRef] -> ProperName -> Bool
isDctorExported _ Nothing _ = True
isDctorExported ident (Just exps) ctor = test `any` exps
  where
  test (PositionedDeclarationRef _ _ d) = test d
  test (TypeRef ident' Nothing) = ident == ident'
  test (TypeRef ident' (Just ctors)) = ident == ident' && ctor `elem` ctors
  test _ = False
