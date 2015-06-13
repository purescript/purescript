{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for rendering documentation generated from PureScript code.

module Language.PureScript.Docs.Render (
  renderModule,
  collectBookmarks
) where

import Control.Monad
import Data.Either
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe)
import Data.List (nub)

import qualified Language.PureScript as P

import Language.PureScript.Docs.RenderedCode
import Language.PureScript.Docs.Types
import Language.PureScript.Docs.Utils.MonoidExtras

-- |
-- Render a single Module.
--
renderModule :: P.Module -> RenderedModule
renderModule m@(P.Module coms moduleName  _ _) =
  RenderedModule (show moduleName) comments declarations
  where
  comments = renderComments coms
  declarations = groupChildren declarationsWithChildren
  declarationsWithChildren = mapMaybe go (P.exportedDeclarations m)
  go decl = getDeclarationTitle decl
             >>= renderDeclaration decl

-- | An intermediate stage which we go through during rendering.
--
-- In the first pass, we take all top level declarations in the module, and
-- render those which should appear at the top level in the output, as well as
-- those which should appear as children of other declarations in the output.
--
-- In the second pass, we move all children under their respective parents,
-- or discard them if none are found.
--
-- This two-pass system is only necessary for type instance declarations, since
-- they appear at the top level in the AST, and since they might need to appear
-- as children in two places (for example, if a data type defined in a module
-- is an instance of a type class also defined in that module).
--
-- This data type is used as an intermediate type between the two stages. The
-- Left case is a child declaration, together with a list of parent declaration
-- titles which this may appear as a child of.
--
-- The Right case is a top level declaration which should pass straight through
-- the second stage; the only way it might change is if child declarations are
-- added to it.
type IntermediateDeclaration
  = Either ([String], RenderedChildDeclaration) RenderedDeclaration

-- | Move child declarations into their respective parents; the second pass.
-- See the comments under the type synonym IntermediateDeclaration for more
-- information.
groupChildren :: [IntermediateDeclaration] -> [RenderedDeclaration]
groupChildren (partitionEithers -> (children, toplevels)) =
  foldl go toplevels children
  where
  go ds (parentTitles, child) =
    map (\d ->
      if rdTitle d `elem` parentTitles
        then d { rdChildren = rdChildren d ++ [child] }
        else d) ds

getDeclarationTitle :: P.Declaration -> Maybe String
getDeclarationTitle (P.TypeDeclaration name _)               = Just (show name)
getDeclarationTitle (P.ExternDeclaration name _)             = Just (show name)
getDeclarationTitle (P.DataDeclaration _ name _ _)           = Just (show name)
getDeclarationTitle (P.ExternDataDeclaration name _)         = Just (show name)
getDeclarationTitle (P.TypeSynonymDeclaration name _ _)      = Just (show name)
getDeclarationTitle (P.TypeClassDeclaration name _ _ _)      = Just (show name)
getDeclarationTitle (P.TypeInstanceDeclaration name _ _ _ _) = Just (show name)
getDeclarationTitle (P.PositionedDeclaration _ _ d)          = getDeclarationTitle d
getDeclarationTitle _                                        = Nothing

basicDeclaration :: String -> RenderedCode -> Maybe IntermediateDeclaration
basicDeclaration title code = Just (Right (RenderedDeclaration title Nothing code Nothing []))

renderDeclaration :: P.Declaration -> String -> Maybe IntermediateDeclaration
renderDeclaration (P.TypeDeclaration ident' ty) title =
  basicDeclaration title code
  where
  code = ident (show ident')
          <> sp <> syntax "::" <> sp
          <> renderType ty
renderDeclaration (P.ExternDeclaration ident' ty) title =
  basicDeclaration title code
  where
  code = ident (show ident')
          <> sp <> syntax "::" <> sp
          <> renderType ty
renderDeclaration (P.DataDeclaration dtype name args ctors) title =
  Just (Right (RenderedDeclaration title Nothing code Nothing children))
  where
  typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
  code = keyword (show dtype) <> sp <> renderType typeApp
  children = map renderCtor ctors
  -- TODO: Comments for data constructors?
  renderCtor (ctor', tys) =
          let typeApp' = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing ctor')) tys
              childCode = renderType typeApp'
          in  RenderedChildDeclaration (show ctor') Nothing childCode Nothing ChildDataConstructor
renderDeclaration (P.ExternDataDeclaration name kind') title =
  basicDeclaration title code
  where
  code = keywordData <> sp
          <> renderType (P.TypeConstructor (P.Qualified Nothing name))
          <> sp <> syntax "::" <> sp
          <> renderKind kind'
renderDeclaration (P.TypeSynonymDeclaration name args ty) title =
  basicDeclaration title code
  where
  typeApp = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
  code = mintersperse sp
          [ keywordType
          , renderType typeApp
          , syntax "="
          , renderType ty
          ]
renderDeclaration (P.TypeClassDeclaration name args implies ds) title = do
  Just (Right (RenderedDeclaration title Nothing code Nothing children))
  where
  code = mintersperse sp $
           [keywordClass]
            ++ maybe [] (:[]) superclasses
            ++ [renderType classApp]
            ++ if (not (null ds)) then [keywordWhere] else []

  superclasses
    | null implies = Nothing
    | otherwise = Just $
        syntax "("
         <> mintersperse (syntax "," <> sp) (map renderImplies implies)
         <> syntax ") <="

  renderImplies (pn, tys) =
    let supApp = foldl P.TypeApp (P.TypeConstructor pn) tys
    in renderType supApp

  classApp = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)

  children = map renderClassMember ds

  -- TODO: Comments for type class members
  renderClassMember (P.PositionedDeclaration _ _ d) = renderClassMember d
  renderClassMember (P.TypeDeclaration ident' ty) =
    let childCode =
          mintersperse sp
            [ ident (show ident')
            , syntax "::"
            , renderType ty
            ]
    in  RenderedChildDeclaration (show ident') Nothing childCode Nothing ChildTypeClassMember
  renderClassMember _ = error "Invalid argument to renderClassMember."
renderDeclaration (P.TypeInstanceDeclaration name constraints className tys _) title = do
  Just (Left (classNameString : typeNameStrings, childDecl))
  where
  classNameString = unQual className
  typeNameStrings = nub (concatMap (P.everythingOnTypes (++) extractProperNames) tys)
  unQual x = let (P.Qualified _ y) = x in show y

  extractProperNames (P.TypeConstructor n) = [unQual n]
  extractProperNames (P.SaturatedTypeSynonym n _) = [unQual n]
  extractProperNames _ = []

  childDecl = RenderedChildDeclaration title Nothing code Nothing ChildInstance

  code =
    mintersperse sp $
      [ keywordInstance
      , ident (show name)
      , syntax "::"
      ] ++ maybe [] (:[]) constraints'
        ++ [ renderType classApp ]

  constraints'
    | null constraints = Nothing
    | otherwise = Just (syntax "(" <> renderedConstraints <> syntax ") =>")

  renderedConstraints = mintersperse (syntax "," <> sp) (map renderConstraint constraints)

  renderConstraint (pn, tys') =
    let supApp = foldl P.TypeApp (P.TypeConstructor pn) tys'
    in renderType supApp

  classApp = foldl P.TypeApp (P.TypeConstructor className) tys
renderDeclaration (P.PositionedDeclaration srcSpan com d') title =
  fmap (addComments . addSourceSpan) (renderDeclaration d' title)
  where
  addComments (Left (t, d)) = Left (t, d { rcdComments = renderComments com })
  addComments (Right d) = Right (d { rdComments = renderComments com })

  addSourceSpan (Left (t, d)) = Left (t, d { rcdSourceSpan = Just srcSpan })
  addSourceSpan (Right d) = Right (d { rdSourceSpan = Just srcSpan })
renderDeclaration _ _ = Nothing

renderComments :: [P.Comment] -> Maybe String
renderComments cs = do
  let raw = concatMap toLines cs
  guard (all hasPipe raw && not (null raw))
  return (go raw)
  where
  go = unlines . map stripPipes

  toLines (P.LineComment s) = [s]
  toLines (P.BlockComment s) = lines s

  hasPipe s = case dropWhile (== ' ') s of { ('|':_) -> True; _ -> False }

  stripPipes = dropPipe . dropWhile (== ' ')

  dropPipe ('|':' ':s) = s
  dropPipe ('|':s) = s
  dropPipe s = s

toTypeVar :: (String, Maybe P.Kind) -> P.Type
toTypeVar (s, Nothing) = P.TypeVar s
toTypeVar (s, Just k) = P.KindedType (P.TypeVar s) k

collectBookmarks :: InPackage P.Module -> [Bookmark]
collectBookmarks (Local m) = map Local (collectBookmarks' m)
collectBookmarks (FromDep pkg m) = map (FromDep pkg) (collectBookmarks' m)

collectBookmarks' :: P.Module -> [(P.ModuleName, String)]
collectBookmarks' m =
  map (P.getModuleName m, )
      (mapMaybe getDeclarationTitle
                (P.exportedDeclarations m))

