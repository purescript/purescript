{-# LANGUAGE RecordWildCards #-}

-- | Functions for creating `RenderedCode` values from data types in
-- Language.PureScript.Docs.Types.
--
-- These functions are the ones that are used in markdown/html documentation
-- generation, but the intention is that you are able to supply your own
-- instead if necessary. For example, the Hoogle input file generator
-- substitutes some of these

module Language.PureScript.Docs.Render where

import Data.Monoid ((<>))
import qualified Language.PureScript as P

import Language.PureScript.Docs.Types
import Language.PureScript.Docs.RenderedCode
import Language.PureScript.Docs.Utils.MonoidExtras

renderDeclaration :: Declaration -> RenderedCode
renderDeclaration Declaration{..} =
  mintersperse sp $ case declInfo of
    ValueDeclaration ty ->
      [ ident declTitle
      , syntax "::"
      , renderType ty
      ]
    DataDeclaration dtype args ->
      [ keyword (show dtype)
      , renderType (typeApp declTitle args)
      ]
    ExternDataDeclaration kind' ->
      [ keywordData
      , renderType (P.TypeConstructor (notQualified declTitle))
      , syntax "::"
      , renderKind kind'
      ]
    TypeSynonymDeclaration args ty ->
      [ keywordType
      , renderType (typeApp declTitle args)
      , syntax "="
      , renderType ty
      ]
    TypeClassDeclaration args implies ->
      [ keywordClass ]
      ++ maybe [] (:[]) superclasses
      ++ [renderType (typeApp declTitle args)]
      ++ if (not (null declChildren)) then [keywordWhere] else []

      where
      superclasses
        | null implies = Nothing
        | otherwise = Just $
            syntax "("
            <> mintersperse (syntax "," <> sp) (map renderConstraint implies)
            <> syntax ")" <> sp <> syntax "<="

renderChildDeclaration :: ChildDeclaration -> RenderedCode
renderChildDeclaration ChildDeclaration{..} =
  mintersperse sp $ case cdeclInfo of
    ChildInstance constraints ty ->
      [ keywordInstance
      , ident cdeclTitle
      , syntax "::"
      ] ++ maybe [] (:[]) constraints'
        ++ [ renderType ty ]
      where
      constraints'
        | null constraints = Nothing
        | otherwise = Just $
              syntax "("
              <> renderedConstraints
              <> syntax ")" <> sp <> syntax "=>"

      renderedConstraints = mintersperse (syntax "," <> sp)
                                         (map renderConstraint constraints)
    ChildDataConstructor args ->
      [ renderType typeApp' ]
      where
      typeApp' = foldl P.TypeApp ctor' args
      ctor' = P.TypeConstructor (notQualified cdeclTitle)

    ChildTypeClassMember ty ->
      [ ident cdeclTitle
      , syntax "::"
      , renderType ty
      ]

renderConstraint :: (P.Qualified P.ProperName, [P.Type]) -> RenderedCode
renderConstraint (pn, tys) =
  renderType $ foldl P.TypeApp (P.TypeConstructor pn) tys

notQualified :: String -> P.Qualified P.ProperName
notQualified = P.Qualified Nothing . P.ProperName

typeApp :: String -> [(String, Maybe P.Kind)] -> P.Type
typeApp title typeArgs =
  foldl P.TypeApp
        (P.TypeConstructor (notQualified title))
        (map toTypeVar typeArgs)

toTypeVar :: (String, Maybe P.Kind) -> P.Type
toTypeVar (s, Nothing) = P.TypeVar s
toTypeVar (s, Just k) = P.KindedType (P.TypeVar s) k
