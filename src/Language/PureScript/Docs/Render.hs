{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for rendering documentation generated from PureScript code.

module Language.PureScript.Docs.Render
  ( renderModule
  , collectBookmarks
  ) where

import Control.Monad
import Control.Category ((>>>))
import Data.Either
import Data.Monoid ((<>))
import Data.Maybe (mapMaybe, isNothing)
import Data.List (nub, isPrefixOf, isSuffixOf)

import qualified Language.PureScript as P

import Language.PureScript.Docs.RenderedCode
import Language.PureScript.Docs.Types
import Language.PureScript.Docs.Utils.MonoidExtras

-- |
-- Render a single Module.
--
renderModule :: P.Module -> RenderedModule
renderModule m@(P.Module coms moduleName  _ _) =
  RenderedModule (show moduleName) comments (declarations m)
  where
  comments = renderComments coms
  declarations =
    P.exportedDeclarations
    >>> mapMaybe (\d -> getDeclarationTitle d >>= renderDeclaration d)
    >>> augmentDeclarations
    >>> map addDefaultFixity

-- | The data type for an intermediate stage which we go through during
-- rendering.
--
-- In the first pass, we take all top level declarations in the module, and
-- collect other information which will later be used to augment the top level
-- declarations. These two situation correspond to the Right and Left
-- constructors, respectively.
--
-- In the second pass, we go over all of the Left values and augment the
-- relevant declarations, leaving only the augmented Right values.
--
-- Note that in the Left case, we provide a [String] as well as augment
-- information. The [String] value should be a list of titles of declarations
-- that the augmentation should apply to. For example, for a type instance
-- declaration, that would be any types or type classes mentioned in the
-- instance. For a fixity declaration, it would be just the relevant operator's
-- name.
type IntermediateDeclaration
  = Either ([String], DeclarationAugment) RenderedDeclaration

-- | Some data which will be used to augment a RenderedDeclaration in the
-- output.
--
-- The AugmentChild constructor allows us to move all children under their
-- respective parents. It is only necessary for type instance declarations,
-- since they appear at the top level in the AST, and since they might need to
-- appear as children in two places (for example, if a data type defined in a
-- module is an instance of a type class also defined in that module).
--
-- The AugmentFixity constructor allows us to augment operator definitions
-- with their associativity and precedence.
data DeclarationAugment
  = AugmentChild RenderedChildDeclaration
  | AugmentFixity P.Fixity

-- | Augment top-level declarations; the second pass. See the comments under
-- the type synonym IntermediateDeclaration for more information.
augmentDeclarations :: [IntermediateDeclaration] -> [RenderedDeclaration]
augmentDeclarations (partitionEithers -> (augments, toplevels)) =
  foldl go toplevels augments
  where
  go ds (parentTitles, a) =
    map (\d ->
      if rdTitle d `elem` parentTitles
        then augmentWith a d
        else d) ds

  augmentWith a d =
    case a of
      AugmentChild child ->
        d { rdChildren = rdChildren d ++ [child] }
      AugmentFixity fixity ->
        d { rdFixity = Just fixity }

-- | Add the default operator fixity for operators which do not have associated
-- fixity declarations.
--
-- TODO: This may no longer be necessary after issue 806 is resolved, hopefully
-- in 0.8.
addDefaultFixity :: RenderedDeclaration -> RenderedDeclaration
addDefaultFixity rd@RenderedDeclaration{..}
  | isOp rdTitle && isNothing rdFixity = rd { rdFixity = Just defaultFixity }
  | otherwise                          = rd
  where
  isOp :: String -> Bool
  isOp str = "(" `isPrefixOf` str && ")" `isSuffixOf` str
  defaultFixity = P.Fixity P.Infixl (-1)

getDeclarationTitle :: P.Declaration -> Maybe String
getDeclarationTitle (P.TypeDeclaration name _)               = Just (show name)
getDeclarationTitle (P.ExternDeclaration name _)             = Just (show name)
getDeclarationTitle (P.DataDeclaration _ name _ _)           = Just (show name)
getDeclarationTitle (P.ExternDataDeclaration name _)         = Just (show name)
getDeclarationTitle (P.TypeSynonymDeclaration name _ _)      = Just (show name)
getDeclarationTitle (P.TypeClassDeclaration name _ _ _)      = Just (show name)
getDeclarationTitle (P.TypeInstanceDeclaration name _ _ _ _) = Just (show name)
getDeclarationTitle (P.FixityDeclaration _ name)             = Just ("(" ++ name ++ ")")
getDeclarationTitle (P.PositionedDeclaration _ _ d)          = getDeclarationTitle d
getDeclarationTitle _                                        = Nothing

-- | Create a basic RenderedDeclaration value.
mkDeclaration :: String -> RenderedCode -> RenderedDeclaration
mkDeclaration title code =
  RenderedDeclaration { rdTitle      = title
                      , rdComments   = Nothing
                      , rdCode       = code
                      , rdSourceSpan = Nothing
                      , rdChildren   = []
                      , rdFixity     = Nothing
                      }

basicDeclaration :: String -> RenderedCode -> Maybe IntermediateDeclaration
basicDeclaration title code = Just $ Right $ mkDeclaration title code

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
  Just (Right (mkDeclaration title code) { rdChildren = children })
  where
  typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
  code = keyword (show dtype) <> sp <> renderType typeApp
  children = map renderCtor ctors
  renderCtor (ctor', tys) =
    RenderedChildDeclaration (show ctor') Nothing Nothing (ChildDataConstructor ctorSignature ctorCode)
    where
    ctor'' = P.TypeConstructor (P.Qualified Nothing ctor')
    typeApp' = foldl P.TypeApp ctor'' tys
    ctorSignature = renderType typeApp'
    ctorCode = ident (show ctor') <> sp <> syntax "::" <> sp <> renderType ctorType
    ctorType = P.quantify $ foldr (\a b -> P.TypeApp (P.TypeApp P.tyFunction a) b) typeApp tys
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
  Just (Right (mkDeclaration title code) { rdChildren = children })
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

  renderClassMember (P.PositionedDeclaration _ _ d) = renderClassMember d
  renderClassMember (P.TypeDeclaration ident' ty) =
    RenderedChildDeclaration (show ident') Nothing Nothing (ChildTypeClassMember contextualType actualType)
    where
    begin               = ident (show ident') <> sp <> syntax "::" <> sp
    contextualType      = begin <> renderType ty
    actualType          = begin <> renderType (addConstraint classConstraint ty)
    classConstraint     = (P.Qualified Nothing name, map toTypeVar args)
    addConstraint c ty' = P.moveQuantifiersToFront (P.quantify (P.ConstrainedType [c] ty'))
  renderClassMember _ = error "Invalid argument to renderClassMember."
renderDeclaration (P.TypeInstanceDeclaration name constraints className tys _) title = do
  Just (Left (classNameString : typeNameStrings, AugmentChild childDecl))
  where
  classNameString = unQual className
  typeNameStrings = nub (concatMap (P.everythingOnTypes (++) extractProperNames) tys)
  unQual x = let (P.Qualified _ y) = x in show y

  extractProperNames (P.TypeConstructor n) = [unQual n]
  extractProperNames (P.SaturatedTypeSynonym n _) = [unQual n]
  extractProperNames _ = []

  childDecl = RenderedChildDeclaration title Nothing Nothing (ChildInstance code)

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
renderDeclaration (P.FixityDeclaration fixity _) title =
  Just (Left ([title], AugmentFixity fixity))
renderDeclaration (P.PositionedDeclaration srcSpan com d') title =
  fmap (addComments . addSourceSpan) (renderDeclaration d' title)
  where
  addComments (Right d) =
    Right (d { rdComments = renderComments com })
  addComments (Left augment) =
    Left (withAugmentChild (\d -> d { rcdComments = renderComments com })
                           augment)

  addSourceSpan (Right d) =
    Right (d { rdSourceSpan = Just srcSpan })
  addSourceSpan (Left augment) =
    Left (withAugmentChild (\d -> d { rcdSourceSpan = Just srcSpan })
                           augment)

  withAugmentChild f (t, a) =
    case a of
      AugmentChild d -> (t, AugmentChild (f d))
      _              -> (t, a)
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

