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
import Control.Monad (void)
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

constraintsAsBox :: TypeRenderOptions -> [Constraint a] -> Box -> Box
constraintsAsBox tro constraints ty = case constraints of
  [con] -> text "(" <> constraintAsBox con `before` (") " <> text doubleRightArrow <> " " <> ty)
  xs -> vcat left (zipWith (\i con -> text (if i == 0 then "( " else ", ") <> constraintAsBox con) [0 :: Int ..] xs) `before` (") " <> text doubleRightArrow <> " " <> ty)
  where
    doubleRightArrow = if troUnicode tro then "⇒" else "=>"

constraintAsBox :: Constraint a -> Box
constraintAsBox (Constraint pn tys _) =
  typeAsBox (foldl (\x y -> TypeApp x y ()) (TypeConstructor (fmap coerceProperName pn) ()) (map void tys))

-- |
-- Generate a pretty-printed string representing a Row
--
prettyPrintRowWith :: TypeRenderOptions -> Char -> Char -> Type a -> Box
prettyPrintRowWith tro open close = uncurry listToBox . toList []
  where
  nameAndTypeToPs :: Char -> Label -> Type a -> Box
  nameAndTypeToPs start name ty = text (start : ' ' : T.unpack (prettyPrintLabel name) ++ " " ++ doubleColon ++ " ") <> typeAsBox ty

  doubleColon = if troUnicode tro then "∷" else "::"

  tailToPs :: Type a -> Box
  tailToPs REmpty{} = nullBox
  tailToPs other = text "| " <> typeAsBox other

  listToBox :: [(Label, Type a)] -> Type a -> Box
  listToBox [] REmpty{} = text [open, close]
  listToBox [] rest = text [ open, ' ' ] <> tailToPs rest <> text [ ' ', close ]
  listToBox ts rest = vcat left $
    zipWith (\(nm, ty) i -> nameAndTypeToPs (if i == 0 then open else ',') nm ty) ts [0 :: Int ..] ++
    [ tailToPs rest, text [close] ]
  toList :: [(Label, Type a)] -> Type a -> ([(Label, Type a)], Type a)
  toList tys (RCons name ty row _) = toList ((name, ty):tys) row
  toList tys r = (reverse tys, r)

prettyPrintRow :: Type a -> String
prettyPrintRow = render . prettyPrintRowWith defaultOptions '(' ')'

typeApp :: Pattern () (Type a) (Type a, Type a)
typeApp = mkPattern match
  where
  match (TypeApp f x _) = Just (f, x)
  match _ = Nothing

appliedFunction :: Pattern () (Type a) (Type a, Type a)
appliedFunction = mkPattern match
  where
  match (PrettyPrintFunction arg ret _) = Just (arg, ret)
  match _ = Nothing

kinded :: Pattern () (Type a) (Kind, Type a)
kinded = mkPattern match
  where
  match (KindedType t k _) = Just (k, t)
  match _ = Nothing

insertPlaceholders :: Type a -> Type a
insertPlaceholders = everywhereOnTypesTopDown convertForAlls . everywhereOnTypes convert
  where
  convert (TypeApp (TypeApp f arg _) ret ann) | void f == tyFunction = PrettyPrintFunction arg ret ann
  convert (TypeApp o r ann) | void o == tyRecord = PrettyPrintObject r ann
  convert other = other
  convertForAlls (ForAll ident ty _ ann) = go [ident] ty
    where
    go idents (ForAll ident' ty' _ _) = go (ident' : idents) ty'
    go idents other = PrettyPrintForAll idents other ann
  convertForAlls other = other

constrained :: Pattern () (Type a) ([Constraint a], Type a)
constrained = mkPattern match
  where
  match (ConstrainedType deps ty _) = Just (deps, ty)
  match _ = Nothing

explicitParens :: Pattern () (Type a) ((), Type a)
explicitParens = mkPattern match
  where
  match (ParensInType ty _) = Just ((), ty)
  match _ = Nothing

matchTypeAtom :: TypeRenderOptions -> Pattern () (Type a) Box
matchTypeAtom tro@TypeRenderOptions{troSuggesting = suggesting} =
    typeLiterals <+> fmap ((`before` (text ")")) . (text "(" <>)) (matchType tro)
  where
    typeLiterals :: Pattern () (Type a) Box
    typeLiterals = mkPattern match where
      match TypeWildcard{} = Just $ text "_"
      match (TypeVar var _) = Just $ text $ T.unpack var
      match (TypeLevelString s _) = Just $ text $ T.unpack $ prettyPrintString s
      match (PrettyPrintObject row _) = Just $ prettyPrintRowWith tro '{' '}' row
      match (TypeConstructor ctor _) = Just $ text $ T.unpack $ runProperName $ disqualify ctor
      match (TUnknown u _)
        | suggesting = Just $ text "_"
        | otherwise = Just $ text $ 't' : show u
      match (Skolem name s _ _ _)
        | suggesting =  Just $ text $ T.unpack name
        | otherwise = Just $ text $ T.unpack name ++ show s
      match REmpty{} = Just $ text "()"
      match row@RCons{} = Just $ prettyPrintRowWith tro '(' ')' row
      match (BinaryNoParensType op l r _) =
        Just $ typeAsBox l <> text " " <> typeAsBox op <> text " " <> typeAsBox r
      match (TypeOp op _) = Just $ text $ T.unpack $ showQualified runOpName op
      match _ = Nothing

matchType :: TypeRenderOptions -> Pattern () (Type a) Box
matchType tro = buildPrettyPrinter operators (matchTypeAtom tro) where
  operators :: OperatorTable () (Type a) Box
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

forall_ :: Pattern () (Type a) ([String], Type a)
forall_ = mkPattern match
  where
  match (PrettyPrintForAll idents ty _) = Just (map T.unpack idents, ty)
  match _ = Nothing

typeAtomAsBox :: Type a -> Box
typeAtomAsBox
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern (matchTypeAtom defaultOptions) ()
  . insertPlaceholders

-- | Generate a pretty-printed string representing a Type, as it should appear inside parentheses
prettyPrintTypeAtom :: Type a -> String
prettyPrintTypeAtom = render . typeAtomAsBox

typeAsBox :: Type a -> Box
typeAsBox = typeAsBoxImpl defaultOptions

suggestedTypeAsBox :: Type a -> Box
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

typeAsBoxImpl :: TypeRenderOptions -> Type a -> Box
typeAsBoxImpl tro
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern (matchType tro) ()
  . insertPlaceholders

-- | Generate a pretty-printed string representing a 'Type'
prettyPrintType :: Type a -> String
prettyPrintType = prettyPrintType' defaultOptions

-- | Generate a pretty-printed string representing a 'Type' using unicode
-- symbols where applicable
prettyPrintTypeWithUnicode :: Type a -> String
prettyPrintTypeWithUnicode = prettyPrintType' unicodeOptions

-- | Generate a pretty-printed string representing a suggested 'Type'
prettyPrintSuggestedType :: Type a -> String
prettyPrintSuggestedType = prettyPrintType' suggestingOptions

prettyPrintType' :: TypeRenderOptions -> Type a -> String
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
