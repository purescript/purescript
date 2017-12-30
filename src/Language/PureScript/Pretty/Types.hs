-- |
-- Pretty printer for Types
--
module Language.PureScript.Pretty.Types
  ( typeAsBox
  , suggestedTypeAsBox
  , prettyPrintType
  , prettyPrintTypeWithUnicode
  , prettyPrintSuggestedType
  , typeAtomAsBox
  , prettyPrintTypeAtom
  , prettyPrintRow
  , prettyPrintLabel
  , prettyPrintObjectKey
  ) where

import Prelude.Compat

import Control.Arrow ((<+>))
import Control.PatternArrows as PA

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Pretty.Common
import Language.PureScript.Pretty.Kinds
import Language.PureScript.Types
import Language.PureScript.PSString (PSString, prettyPrintString, decodeString)
import Language.PureScript.Label (Label(..))

import Text.PrettyPrint.Boxes hiding ((<+>))

-- TODO(Christoph): get rid of T.unpack s

constraintsAsBox :: TypeRenderOptions -> Constraint -> Box -> Box
constraintsAsBox tro con ty =
    constraintAsBox con `before` (" " <> text doubleRightArrow <> " " <> ty)
  where
    doubleRightArrow = if troUnicode tro then "⇒" else "=>"

constraintAsBox :: Constraint -> Box
constraintAsBox (Constraint pn tys _) = typeAsBox (foldl TypeApp (TypeConstructor (fmap coerceProperName pn)) tys)

-- |
-- Generate a pretty-printed string representing a Row
--
prettyPrintRowWith :: TypeRenderOptions -> Char -> Char -> Type -> Box
prettyPrintRowWith tro open close = uncurry listToBox . toList []
  where
  nameAndTypeToPs :: Char -> Label -> Type -> Box
  nameAndTypeToPs start name ty = text (start : ' ' : T.unpack (prettyPrintLabel name) ++ " " ++ doubleColon ++ " ") <> typeAsBox ty

  doubleColon = if troUnicode tro then "∷" else "::"

  tailToPs :: Type -> Box
  tailToPs REmpty = nullBox
  tailToPs other = text "| " <> typeAsBox other

  listToBox :: [(Label, Type)] -> Type -> Box
  listToBox [] REmpty = text [open, close]
  listToBox [] rest = text [ open, ' ' ] <> tailToPs rest <> text [ ' ', close ]
  listToBox ts rest = vcat left $
    zipWith (\(nm, ty) i -> nameAndTypeToPs (if i == 0 then open else ',') nm ty) ts [0 :: Int ..] ++
    [ tailToPs rest, text [close] ]
  toList :: [(Label, Type)] -> Type -> ([(Label, Type)], Type)
  toList tys (RCons name ty row) = toList ((name, ty):tys) row
  toList tys r = (reverse tys, r)

prettyPrintRow :: Type -> String
prettyPrintRow = render . prettyPrintRowWith defaultOptions '(' ')'

typeApp :: Pattern () Type (Type, Type)
typeApp = mkPattern match
  where
  match (TypeApp f x) = Just (f, x)
  match _ = Nothing

appliedFunction :: Pattern () Type (Type, Type)
appliedFunction = mkPattern match
  where
  match (PrettyPrintFunction arg ret) = Just (arg, ret)
  match _ = Nothing

kinded :: Pattern () Type (Kind, Type)
kinded = mkPattern match
  where
  match (KindedType t k) = Just (k, t)
  match _ = Nothing

insertPlaceholders :: Type -> Type
insertPlaceholders = everywhereOnTypesTopDown convertForAlls . everywhereOnTypes convert
  where
  convert (TypeApp (TypeApp f arg) ret) | f == tyFunction = PrettyPrintFunction arg ret
  convert (TypeApp o r) | o == tyRecord = PrettyPrintObject r
  convert other = other
  convertForAlls (ForAll ident ty _) = go [ident] ty
    where
    go idents (ForAll ident' ty' _) = go (ident' : idents) ty'
    go idents other = PrettyPrintForAll idents other
  convertForAlls other = other

constrained :: Pattern () Type (Constraint, Type)
constrained = mkPattern match
  where
  match (ConstrainedType deps ty) = Just (deps, ty)
  match _ = Nothing

explicitParens :: Pattern () Type ((), Type)
explicitParens = mkPattern match
  where
  match (ParensInType ty) = Just ((), ty)
  match _ = Nothing

matchTypeAtom :: TypeRenderOptions -> Pattern () Type Box
matchTypeAtom tro@TypeRenderOptions{troSuggesting = suggesting} =
    typeLiterals <+> fmap ((`before` (text ")")) . (text "(" <>)) (matchType tro)
  where
    typeLiterals :: Pattern () Type Box
    typeLiterals = mkPattern match where
      match TypeWildcard{} = Just $ text "_"
      match (TypeVar var) = Just $ text $ T.unpack var
      match (TypeLevelString s) = Just $ text $ T.unpack $ prettyPrintString s
      match (PrettyPrintObject row) = Just $ prettyPrintRowWith tro '{' '}' row
      match (TypeConstructor ctor) = Just $ text $ T.unpack $ runProperName $ disqualify ctor
      match (TUnknown u)
        | suggesting = Just $ text "_"
        | otherwise = Just $ text $ 't' : show u
      match (Skolem name s _ _)
        | suggesting =  Just $ text $ T.unpack name
        | otherwise = Just $ text $ T.unpack name ++ show s
      match REmpty = Just $ text "()"
      match row@RCons{} = Just $ prettyPrintRowWith tro '(' ')' row
      match (ProxyType t) = Just $ text "@" <> typeAtomAsBox t
      match (BinaryNoParensType op l r) =
        Just $ typeAsBox l <> text " " <> typeAsBox op <> text " " <> typeAsBox r
      match (TypeOp op) = Just $ text $ T.unpack $ showQualified runOpName op
      match _ = Nothing

matchType :: TypeRenderOptions -> Pattern () Type Box
matchType tro = buildPrettyPrinter operators (matchTypeAtom tro) where
  operators :: OperatorTable () Type Box
  operators =
    OperatorTable [ [ AssocL typeApp $ \f x -> keepSingleLinesOr (moveRight 2) f x ]
                  , [ AssocR appliedFunction $ \arg ret -> keepSingleLinesOr id arg (text rightArrow <> " " <> ret) ]
                  , [ Wrap constrained $ \deps ty -> constraintsAsBox tro deps ty ]
                  , [ Wrap forall_ $ \idents ty -> keepSingleLinesOr (moveRight 2) (text (forall' ++ " " ++ unwords idents ++ ".")) ty ]
                  , [ Wrap kinded $ \k ty -> keepSingleLinesOr (moveRight 2) ty (text (doubleColon ++ " " ++ T.unpack (prettyPrintKind k))) ]
                  , [ Wrap explicitParens $ \_ ty -> ty ]
                  ]

  rightArrow = if troUnicode tro then "→" else "->"
  forall' = if troUnicode tro then "∀" else "forall"
  doubleColon = if troUnicode tro then "∷" else "::"

  -- If both boxes span a single line, keep them on the same line, or else
  -- use the specified function to modify the second box, then combine vertically.
  keepSingleLinesOr :: (Box -> Box) -> Box -> Box -> Box
  keepSingleLinesOr f b1 b2
    | rows b1 > 1 || rows b2 > 1 = vcat left [ b1, f b2 ]
    | otherwise = hcat top [ b1, text " ", b2]

forall_ :: Pattern () Type ([String], Type)
forall_ = mkPattern match
  where
  match (PrettyPrintForAll idents ty) = Just (map T.unpack idents, ty)
  match _ = Nothing

typeAtomAsBox :: Type -> Box
typeAtomAsBox
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern (matchTypeAtom defaultOptions) ()
  . insertPlaceholders

-- | Generate a pretty-printed string representing a Type, as it should appear inside parentheses
prettyPrintTypeAtom :: Type -> String
prettyPrintTypeAtom = render . typeAtomAsBox

typeAsBox :: Type -> Box
typeAsBox = typeAsBoxImpl defaultOptions

suggestedTypeAsBox :: Type -> Box
suggestedTypeAsBox = typeAsBoxImpl suggestingOptions

data TypeRenderOptions = TypeRenderOptions
  { troSuggesting :: Bool
  , troUnicode :: Bool
  }

suggestingOptions :: TypeRenderOptions
suggestingOptions = TypeRenderOptions True False

defaultOptions :: TypeRenderOptions
defaultOptions = TypeRenderOptions False False

unicodeOptions :: TypeRenderOptions
unicodeOptions = TypeRenderOptions False True

typeAsBoxImpl :: TypeRenderOptions -> Type -> Box
typeAsBoxImpl tro
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern (matchType tro) ()
  . insertPlaceholders

-- | Generate a pretty-printed string representing a 'Type'
prettyPrintType :: Type -> String
prettyPrintType = prettyPrintType' defaultOptions

-- | Generate a pretty-printed string representing a 'Type' using unicode
-- symbols where applicable
prettyPrintTypeWithUnicode :: Type -> String
prettyPrintTypeWithUnicode = prettyPrintType' unicodeOptions

-- | Generate a pretty-printed string representing a suggested 'Type'
prettyPrintSuggestedType :: Type -> String
prettyPrintSuggestedType = prettyPrintType' suggestingOptions

prettyPrintType' :: TypeRenderOptions -> Type -> String
prettyPrintType' tro = render . typeAsBoxImpl tro

prettyPrintLabel :: Label -> Text
prettyPrintLabel (Label s) =
  case decodeString s of
    Just s' | not (objectKeyRequiresQuoting s') ->
      s'
    _ ->
      prettyPrintString s

prettyPrintObjectKey :: PSString -> Text
prettyPrintObjectKey = prettyPrintLabel . Label
