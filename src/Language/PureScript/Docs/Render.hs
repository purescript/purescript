-- |
-- Functions for creating `RenderedCode` values from data types in
-- Language.PureScript.Docs.Types.
--
-- These functions are the ones that are used in markdown/html documentation
-- generation, but the intention is that you are able to supply your own
-- instead if necessary. For example, the Hoogle input file generator
-- substitutes some of these

module Language.PureScript.Docs.Render where

import Prelude

import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T

import Language.PureScript.Docs.RenderedCode.RenderType ( renderType, renderTypeAtom, renderTypeWithRole )
import Language.PureScript.Docs.RenderedCode.Types ( alias, aliasName, dataCtor, ident, keyword, keywordAs, keywordClass, keywordData, keywordFixity, keywordType, keywordWhere, sp, syntax, typeVar, RenderedCode )
import Language.PureScript.Docs.Types ( isTypeClassMember, kindSignatureForKeyword, ChildDeclaration(..), ChildDeclarationInfo(ChildTypeClassMember, ChildInstance, ChildDataConstructor), Constraint', Declaration(..), DeclarationInfo(AliasDeclaration, ValueDeclaration, DataDeclaration, ExternDataDeclaration, TypeSynonymDeclaration, TypeClassDeclaration), KindInfo(..), Type' )
import Language.PureScript.Docs.Utils.MonoidExtras ( mintersperse )

import Language.PureScript.AST.Operators qualified as ASTO
import Language.PureScript.Environment qualified as PEnv
import Language.PureScript.Names qualified as PN
import Language.PureScript.Types qualified as PT

renderKindSig :: Text -> KindInfo -> RenderedCode
renderKindSig declTitle KindInfo{..} =
  mintersperse sp
      [ keyword $ kindSignatureForKeyword kiKeyword
      , renderType (PT.TypeConstructor () (notQualified declTitle))
      , syntax "::"
      , renderType kiKind
      ]

renderDeclaration :: Declaration -> RenderedCode
renderDeclaration Declaration{..} =
  mintersperse sp $ case declInfo of
    ValueDeclaration ty ->
      [ ident' declTitle
      , syntax "::"
      , renderType ty
      ]
    DataDeclaration dtype args roles ->
      [ keyword (PEnv.showDataDeclType dtype)
      , renderTypeWithRole roles (typeApp declTitle args)
      ]

    -- All FFI declarations, except for `Prim` modules' doc declarations,
    -- will have been converted to `DataDeclaration`s by this point.
    ExternDataDeclaration kind' _ ->
      [ keywordData
      , renderType (PT.TypeConstructor () (notQualified declTitle))
      , syntax "::"
      , renderType kind'
      ]
    TypeSynonymDeclaration args ty ->
      [ keywordType
      , renderType (typeApp declTitle args)
      , syntax "="
      , renderType ty
      ]
    TypeClassDeclaration args implies fundeps ->
      [ keywordClass ]
      ++ maybeToList superclasses
      ++ [renderType (typeApp declTitle args)]
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

    AliasDeclaration (ASTO.Fixity associativity precedence) for ->
      [ keywordFixity associativity
      , syntax $ T.pack $ show precedence
      , alias for
      , keywordAs
      , aliasName for declTitle
      ]

renderChildDeclaration :: ChildDeclaration -> RenderedCode
renderChildDeclaration ChildDeclaration{..} =
  mintersperse sp $ case cdeclInfo of
    ChildInstance constraints ty ->
      maybeToList (renderConstraints constraints) ++ [ renderType ty ]
    ChildDataConstructor args ->
      dataCtor' cdeclTitle : map renderTypeAtom args

    ChildTypeClassMember ty ->
      [ ident' cdeclTitle
      , syntax "::"
      , renderType ty
      ]

renderConstraint :: Constraint' -> RenderedCode
renderConstraint (PT.Constraint ann pn kinds tys _) =
  renderType $ foldl (PT.TypeApp ann) (foldl (PT.KindApp ann) (PT.TypeConstructor ann (fmap PN.coerceProperName pn)) kinds) tys

renderConstraints :: [Constraint'] -> Maybe RenderedCode
renderConstraints constraints
  | null constraints = Nothing
  | otherwise = Just $
        syntax "("
        <> renderedConstraints
        <> syntax ")" <> sp <> syntax "=>"
  where
  renderedConstraints =
    mintersperse (syntax "," <> sp)
                 (map renderConstraint constraints)

notQualified :: Text -> PN.Qualified (PN.ProperName a)
notQualified = PN.Qualified PN.ByNullSourcePos . PN.ProperName

ident' :: Text -> RenderedCode
ident' = ident . PN.Qualified PN.ByNullSourcePos . PN.Ident

dataCtor' :: Text -> RenderedCode
dataCtor' = dataCtor . notQualified

typeApp :: Text -> [(Text, Maybe Type')] -> Type'
typeApp title typeArgs =
  foldl (PT.TypeApp ())
        (PT.TypeConstructor () (notQualified title))
        (map toTypeVar typeArgs)

toTypeVar :: (Text, Maybe Type') -> Type'
toTypeVar (s, Nothing) = PT.TypeVar () s
toTypeVar (s, Just k) = PT.KindedType () (PT.TypeVar () s) k
