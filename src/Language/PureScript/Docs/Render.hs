-- |
-- Functions for creating `RenderedCode` values from data types in
-- Language.PureScript.Docs.Types.
--
-- These functions are the ones that are used in markdown/html documentation
-- generation, but the intention is that you are able to supply your own
-- instead if necessary. For example, the Hoogle input file generator
-- substitutes some of these

module Language.PureScript.Docs.Render where

import Prelude.Compat

import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Docs.RenderedCode
import Language.PureScript.Docs.Types
import Language.PureScript.Docs.Utils.MonoidExtras
import qualified Language.PureScript as P

renderDeclaration :: Declaration -> RenderedCode
renderDeclaration = renderDeclarationWithOptions defaultRenderTypeOptions

renderDeclarationWithOptions :: RenderTypeOptions -> Declaration -> RenderedCode
renderDeclarationWithOptions opts Declaration{..} =
  mintersperse sp $ case declInfo of
    ValueDeclaration ty ->
      [ ident' declTitle
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
    TypeClassDeclaration args implies fundeps ->
      [ keywordClass ]
      ++ maybeToList superclasses
      ++ [renderType' (typeApp declTitle args)]
      ++ fundepsList
      ++ [keywordWhere | any isTypeClassMember declChildren]

      where
      superclasses
        | null implies = Nothing
        | otherwise = Just $
            syntax "("
            <> mintersperse (syntax "," <> sp) (map renderConstraint implies)
            <> syntax ")" <> sp <> syntax "<="

      fundepsList =
           [syntax "|" | not (null fundeps)]
        ++ [mintersperse
             (syntax "," <> sp)
             [typeVars from <> sp <> syntax "->" <> sp <> typeVars to | (from, to) <- fundeps ]
           ]
        where
          typeVars = mintersperse sp . map typeVar

    AliasDeclaration (P.Fixity associativity precedence) for ->
      [ keywordFixity associativity
      , syntax $ T.pack $ show precedence
      , alias for
      , keywordAs
      , aliasName for declTitle
      ]

    ExternKindDeclaration ->
      [ keywordKind
      , kind (notQualified declTitle)
      ]

  where
  renderType' :: P.Type -> RenderedCode
  renderType' = renderTypeWithOptions opts

renderChildDeclaration :: ChildDeclaration -> RenderedCode
renderChildDeclaration = renderChildDeclarationWithOptions defaultRenderTypeOptions

renderChildDeclarationWithOptions :: RenderTypeOptions -> ChildDeclaration -> RenderedCode
renderChildDeclarationWithOptions opts ChildDeclaration{..} =
  mintersperse sp $ case cdeclInfo of
    ChildInstance constraints ty ->
      maybeToList (renderConstraints constraints) ++ [ renderType' ty ]
    ChildDataConstructor args ->
      [ dataCtor' cdeclTitle ]
      ++ map renderTypeAtom' args

    ChildTypeClassMember ty ->
      [ ident' cdeclTitle
      , syntax "::"
      , renderType' ty
      ]
  where
  renderType' = renderTypeWithOptions opts
  renderTypeAtom' = renderTypeAtomWithOptions opts

renderConstraint :: P.Constraint -> RenderedCode
renderConstraint = renderConstraintWithOptions defaultRenderTypeOptions

renderConstraintWithOptions :: RenderTypeOptions -> P.Constraint -> RenderedCode
renderConstraintWithOptions opts (P.Constraint pn tys _) =
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

notQualified :: Text -> P.Qualified (P.ProperName a)
notQualified = P.Qualified Nothing . P.ProperName

ident' :: Text -> RenderedCode
ident' = ident . P.Qualified Nothing . P.Ident

dataCtor' :: Text -> RenderedCode
dataCtor' = dataCtor . notQualified

typeApp :: Text -> [(Text, Maybe P.Kind)] -> P.Type
typeApp title typeArgs =
  foldl P.TypeApp
        (P.TypeConstructor (notQualified title))
        (map toTypeVar typeArgs)

toTypeVar :: (Text, Maybe P.Kind) -> P.Type
toTypeVar (s, Nothing) = P.TypeVar s
toTypeVar (s, Just k) = P.KindedType (P.TypeVar s) k
