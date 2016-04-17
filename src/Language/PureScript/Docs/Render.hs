{-# LANGUAGE RecordWildCards #-}

-- | Functions for creating `RenderedCode` values from data types in
-- Language.PureScript.Docs.Types.
--
-- These functions are the ones that are used in markdown/html documentation
-- generation, but the intention is that you are able to supply your own
-- instead if necessary. For example, the Hoogle input file generator
-- substitutes some of these

module Language.PureScript.Docs.Render where

import Data.Maybe (maybeToList)
import Data.Monoid ((<>))
import qualified Language.PureScript as P

import Language.PureScript.Docs.Types
import Language.PureScript.Docs.RenderedCode
import Language.PureScript.Docs.Utils.MonoidExtras

renderDeclaration :: Declaration -> RenderedCode
renderDeclaration = renderDeclarationWithOptions defaultRenderTypeOptions

renderDeclarationWithOptions :: RenderTypeOptions -> Declaration -> RenderedCode
renderDeclarationWithOptions opts Declaration{..} =
  mintersperse sp $ case declInfo of
    ValueDeclaration ty ->
      [ ident declTitle
      , syntax "::"
      , renderType' ty
      ]
    DataDeclaration dtype args ->
      [ keyword (P.showDataDeclType dtype)
      , renderType' (typeApp declTitle args)
      ]
    ExternDataDeclaration kind' ->
      [ keywordData
      , renderType' (P.TypeConstructor (notQualified declTitle))
      , syntax "::"
      , renderKind kind'
      ]
    TypeSynonymDeclaration args ty ->
      [ keywordType
      , renderType' (typeApp declTitle args)
      , syntax "="
      , renderType' ty
      ]
    TypeClassDeclaration args implies ->
      [ keywordClass ]
      ++ maybeToList superclasses
      ++ [renderType' (typeApp declTitle args)]
      ++ [keywordWhere | any isTypeClassMember declChildren]

      where
      superclasses
        | null implies = Nothing
        | otherwise = Just $
            syntax "("
            <> mintersperse (syntax "," <> sp) (map renderConstraint implies)
            <> syntax ")" <> sp <> syntax "<="
    AliasDeclaration for@(P.Qualified _ alias) (P.Fixity associativity precedence) ->
      [ keywordFixity associativity
      , syntax $ show precedence
      , ident $ renderAlias for
      , keyword "as"
      , ident $ adjustAliasName alias declTitle
      ]

  where
  renderType' = renderTypeWithOptions opts
  renderAlias (P.Qualified mn alias)
    | mn == currentModule opts =
        P.foldFixityAlias P.runIdent P.runProperName (("type " ++) . P.runProperName) alias
    | otherwise =
        P.foldFixityAlias
          (P.showQualified P.runIdent . P.Qualified mn)
          (P.showQualified P.runProperName . P.Qualified mn)
          (("type " ++) . P.showQualified P.runProperName . P.Qualified mn)
          alias

  adjustAliasName (P.AliasType{}) title = drop 6 (init title)
  adjustAliasName _ title = tail (init title)

renderChildDeclaration :: ChildDeclaration -> RenderedCode
renderChildDeclaration = renderChildDeclarationWithOptions defaultRenderTypeOptions

renderChildDeclarationWithOptions :: RenderTypeOptions -> ChildDeclaration -> RenderedCode
renderChildDeclarationWithOptions opts ChildDeclaration{..} =
  mintersperse sp $ case cdeclInfo of
    ChildInstance constraints ty ->
      maybeToList (renderConstraints constraints) ++ [ renderType' ty ]
    ChildDataConstructor args ->
      [ renderType' typeApp' ]
      where
      typeApp' = foldl P.TypeApp ctor' args
      ctor' = P.TypeConstructor (notQualified cdeclTitle)

    ChildTypeClassMember ty ->
      [ ident cdeclTitle
      , syntax "::"
      , renderType' ty
      ]
  where
  renderType' = renderTypeWithOptions opts

renderConstraint :: P.Constraint -> RenderedCode
renderConstraint = renderConstraintWithOptions defaultRenderTypeOptions

renderConstraintWithOptions :: RenderTypeOptions -> P.Constraint -> RenderedCode
renderConstraintWithOptions opts (pn, tys) =
  renderTypeWithOptions opts $ foldl P.TypeApp (P.TypeConstructor (fmap P.coerceProperName pn)) tys

renderConstraints :: [P.Constraint] -> Maybe RenderedCode
renderConstraints = renderConstraintsWithOptions defaultRenderTypeOptions

renderConstraintsWithOptions :: RenderTypeOptions -> [P.Constraint] -> Maybe RenderedCode
renderConstraintsWithOptions opts constraints
  | null constraints = Nothing
  | otherwise = Just $
        syntax "("
        <> renderedConstraints
        <> syntax ")" <> sp <> syntax "=>"
  where
  renderedConstraints =
    mintersperse (syntax "," <> sp)
                 (map (renderConstraintWithOptions opts) constraints)

notQualified :: String -> P.Qualified (P.ProperName a)
notQualified = P.Qualified Nothing . P.ProperName

typeApp :: String -> [(String, Maybe P.Kind)] -> P.Type
typeApp title typeArgs =
  foldl P.TypeApp
        (P.TypeConstructor (notQualified title))
        (map toTypeVar typeArgs)

toTypeVar :: (String, Maybe P.Kind) -> P.Type
toTypeVar (s, Nothing) = P.TypeVar s
toTypeVar (s, Just k) = P.KindedType (P.TypeVar s) k
