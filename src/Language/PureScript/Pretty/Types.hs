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

import PSPrelude

import Control.Arrow ((<+>))
import Control.PatternArrows as PA

import Data.String (String)
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

import qualified Text.PrettyPrint.Boxes as B

type Box = B.Box

renderT :: Box -> Text
renderT = toS . B.render

text :: String -> Box
text = B.text

textT :: Text -> Box
textT = text . toS

moveRight :: Int -> Box -> Box
moveRight = B.moveRight

nullBox :: B.Box
nullBox = B.nullBox

constraintsAsBox :: TypeRenderOptions -> Constraint -> Box -> Box
constraintsAsBox tro con ty =
    constraintAsBox con `before` (" " B.<> text doubleRightArrow B.<> " " B.<> ty)
  where
    doubleRightArrow = if troUnicode tro then "⇒" else "=>"

constraintAsBox :: Constraint -> Box
constraintAsBox (Constraint pn tys _) = typeAsBox (foldl TypeApp (TypeConstructor (fmap coerceProperName pn)) tys)

-- |
-- Generate a pretty-printed string representing a Row
--
prettyPrintRowWith :: TypeRenderOptions -> Char -> Char -> Type -> Box
prettyPrintRowWith tro open close = uncurry listToBox . toLst []
  where
  nameAndTypeToPs :: Char -> Label -> Type -> Box
  nameAndTypeToPs start name ty = textT (T.cons start $ T.cons ' ' $ prettyPrintLabel name <> " " <> doubleColon <> " ") B.<> typeAsBox ty

  doubleColon = if troUnicode tro then "∷" else "::"

  tailToPs :: Type -> Box
  tailToPs REmpty = nullBox
  tailToPs other = text "| " B.<> typeAsBox other

  listToBox :: [(Label, Type)] -> Type -> Box
  listToBox [] REmpty = text [open, close]
  listToBox [] rest = text [ open, ' ' ] B.<> tailToPs rest B.<> text [ ' ', close ]
  listToBox ts rest = B.vcat B.left $
    zipWith (\(nm, ty) i -> nameAndTypeToPs (if i == 0 then open else ',') nm ty) ts [0 :: Int ..] ++
    [ tailToPs rest, text [close] ]
  toLst :: [(Label, Type)] -> Type -> ([(Label, Type)], Type)
  toLst tys (RCons name ty row) = toLst ((name, ty):tys) row
  toLst tys r = (reverse tys, r)

prettyPrintRow :: Type -> Text
prettyPrintRow = renderT . prettyPrintRowWith defaultOptions '(' ')'

-- Treat `ProxyType t` in a similar way to the application of a type
-- constructor `@` to `t`, i.e: `@ t`, except that we don't render the unneeded
-- space. So we end up with `@t`.
proxyType :: Pattern () Type (Type, Type)
proxyType = mkPattern match
  where
  match (ProxyType t) = Just (TypeConstructor (Qualified Nothing (ProperName "@")), t)
  match _ = Nothing

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
    typeLiterals <+> fmap ((`before` (text ")")) . (text "(" B.<>)) (matchType tro)
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
      match (BinaryNoParensType op l r) =
        Just $ typeAsBox l B.<> text " " B.<> typeAsBox op B.<> text " " B.<> typeAsBox r
      match (TypeOp op) = Just $ text $ T.unpack $ showQualified runOpName op
      match _ = Nothing

matchType :: TypeRenderOptions -> Pattern () Type Box
matchType tro = buildPrettyPrinter operators (matchTypeAtom tro) where
  operators :: OperatorTable () Type Box
  operators =
    OperatorTable [ [ AssocL proxyType $ \p ty -> p B.<> ty ]
                  , [ AssocL typeApp $ \f x -> keepSingleLinesOr (moveRight 2) f x ]
                  , [ AssocR appliedFunction $ \arg ret -> keepSingleLinesOr id arg (textT rightArrow B.<> " " B.<> ret) ]
                  , [ Wrap constrained $ \deps ty -> constraintsAsBox tro deps ty ]
                  , [ Wrap forall_ $ \idents ty -> keepSingleLinesOr (moveRight 2) (textT (forall' <> " " <> T.unwords idents <> ".")) ty ]
                  , [ Wrap kinded $ \k ty -> keepSingleLinesOr (moveRight 2) ty (textT (doubleColon <> " " <> prettyPrintKind k)) ]
                  , [ Wrap explicitParens $ \_ ty -> ty ]
                  ]

  rightArrow = if troUnicode tro then "→" else "->"
  forall' = if troUnicode tro then "∀" else "forall"
  doubleColon = if troUnicode tro then "∷" else "::"

  -- If both boxes span a single line, keep them on the same line, or else
  -- use the specified function to modify the second box, then combine vertically.
  keepSingleLinesOr :: (Box -> Box) -> Box -> Box -> Box
  keepSingleLinesOr f b1 b2
    | B.rows b1 > 1 || B.rows b2 > 1 = b1 B.// f b2
    | otherwise = B.hcat B.top [ b1, text " ", b2]

forall_ :: Pattern () Type ([Text], Type)
forall_ = mkPattern match
  where
  match (PrettyPrintForAll idents ty) = Just (idents, ty)
  match _ = Nothing

typeAtomAsBox :: Type -> Box
typeAtomAsBox
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern (matchTypeAtom defaultOptions) ()
  . insertPlaceholders

-- | Generate a pretty-printed string representing a Type, as it should appear inside parentheses
prettyPrintTypeAtom :: Type -> Text
prettyPrintTypeAtom = renderT . typeAtomAsBox

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
prettyPrintType :: Type -> Text
prettyPrintType = prettyPrintType' defaultOptions

-- | Generate a pretty-printed string representing a 'Type' using unicode
-- symbols where applicable
prettyPrintTypeWithUnicode :: Type -> Text
prettyPrintTypeWithUnicode = prettyPrintType' unicodeOptions

-- | Generate a pretty-printed string representing a suggested 'Type'
prettyPrintSuggestedType :: Type -> Text
prettyPrintSuggestedType = prettyPrintType' suggestingOptions

prettyPrintType' :: TypeRenderOptions -> Type -> Text
prettyPrintType' tro = renderT . typeAsBoxImpl tro

prettyPrintLabel :: Label -> Text
prettyPrintLabel (Label s) =
  case decodeString s of
    Just s' | not (objectKeyRequiresQuoting s') ->
      s'
    _ ->
      prettyPrintString s

prettyPrintObjectKey :: PSString -> Text
prettyPrintObjectKey = prettyPrintLabel . Label
