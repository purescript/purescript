-- |
-- Pretty printer for Types
--
module Language.PureScript.Pretty.Types
  ( PrettyPrintType(..)
  , PrettyPrintConstraint
  , convertPrettyPrintType
  , typeAsBox
  , typeAsBox'
  , suggestedTypeAsBox
  , prettyPrintType
  , prettyPrintTypeWithUnicode
  , prettyPrintSuggestedType
  , typeAtomAsBox
  , typeAtomAsBox'
  , prettyPrintTypeAtom
  , prettyPrintRow
  , prettyPrintLabel
  , prettyPrintObjectKey
  ) where

import Prelude.Compat hiding ((<>))

import Control.Arrow ((<+>))
import Control.PatternArrows as PA

import Data.Functor (($>))
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

data PrettyPrintType
  = PPTUnknown Int
  | PPTypeVar Text
  | PPTypeLevelString PSString
  | PPTypeWildcard (Maybe Text)
  | PPTypeConstructor (Qualified (ProperName 'TypeName))
  | PPTypeOp (Qualified (OpName 'TypeOpName))
  | PPSkolem Text Int
  | PPTypeApp PrettyPrintType PrettyPrintType
  | PPConstrainedType PrettyPrintConstraint PrettyPrintType
  | PPKindedType PrettyPrintType (Kind ())
  | PPBinaryNoParensType PrettyPrintType PrettyPrintType PrettyPrintType
  | PPParensInType PrettyPrintType
  | PPForAll [Text] PrettyPrintType
  | PPFunction PrettyPrintType PrettyPrintType
  | PPRecord PrettyPrintType
  | PPRCons Label PrettyPrintType PrettyPrintType
  | PPREmpty

type PrettyPrintConstraint = (Qualified (ProperName 'ClassName), [PrettyPrintType])

convertPrettyPrintType :: Type a -> PrettyPrintType
convertPrettyPrintType = go
  where
  go (TUnknown _ n) = PPTUnknown n
  go (TypeVar _ t) = PPTypeVar t
  go (TypeLevelString _ s) = PPTypeLevelString s
  go (TypeWildcard _ n) = PPTypeWildcard n
  go (TypeConstructor _ c) = PPTypeConstructor c
  go (TypeOp _ o) = PPTypeOp o
  go (Skolem _ t n _) = PPSkolem t n
  go (ConstrainedType _ (Constraint _ cls args _) ty) = PPConstrainedType (cls, go <$> args) (go ty)
  go (KindedType _ ty k) = PPKindedType (go ty) (k $> ())
  go (BinaryNoParensType _ ty1 ty2 ty3) = PPBinaryNoParensType (go ty1) (go ty2) (go ty3)
  go (ParensInType _ ty) = PPParensInType (go ty)
  go (REmpty _) = PPREmpty
  go (RCons _ l ty1 ty2) = PPRCons l (go ty1) (go ty2)
  go (ForAll _ v ty _) = goForAll [v] ty
  go (TypeApp _ (TypeApp _ f arg) ret) | eqType f tyFunction = PPFunction (go arg) (go ret)
  go (TypeApp _ o r) | eqType o tyRecord = PPRecord (go r)
  go (TypeApp _ a b) = PPTypeApp (go a) (go b)

  goForAll vs (ForAll _ v ty _) = goForAll (v : vs) ty
  goForAll vs ty = PPForAll vs (go ty)

-- TODO(Christoph): get rid of T.unpack s

constraintsAsBox :: TypeRenderOptions -> PrettyPrintConstraint -> Box -> Box
constraintsAsBox tro con ty =
    constraintAsBox con `before` (" " <> text doubleRightArrow <> " " <> ty)
  where
    doubleRightArrow = if troUnicode tro then "⇒" else "=>"

constraintAsBox :: PrettyPrintConstraint -> Box
constraintAsBox (pn, tys) = typeAsBox' (foldl PPTypeApp (PPTypeConstructor (fmap coerceProperName pn)) tys)

-- |
-- Generate a pretty-printed string representing a Row
--
prettyPrintRowWith :: TypeRenderOptions -> Char -> Char -> PrettyPrintType -> Box
prettyPrintRowWith tro open close = uncurry listToBox . toList []
  where
  nameAndTypeToPs :: Char -> Label -> PrettyPrintType -> Box
  nameAndTypeToPs start name ty = text (start : ' ' : T.unpack (prettyPrintLabel name) ++ " " ++ doubleColon ++ " ") <> typeAsBox' ty

  doubleColon = if troUnicode tro then "∷" else "::"

  tailToPs :: PrettyPrintType -> Box
  tailToPs PPREmpty = nullBox
  tailToPs other = text "| " <> typeAsBox' other

  listToBox :: [(Label, PrettyPrintType)] -> PrettyPrintType -> Box
  listToBox [] PPREmpty = text [open, close]
  listToBox [] rest = text [ open, ' ' ] <> tailToPs rest <> text [ ' ', close ]
  listToBox ts rest = vcat left $
    zipWith (\(nm, ty) i -> nameAndTypeToPs (if i == 0 then open else ',') nm ty) ts [0 :: Int ..] ++
    [ tailToPs rest, text [close] ]
  toList :: [(Label, PrettyPrintType)] -> PrettyPrintType -> ([(Label, PrettyPrintType)], PrettyPrintType)
  toList tys (PPRCons name ty row) = toList ((name, ty):tys) row
  toList tys r = (reverse tys, r)

prettyPrintRow :: PrettyPrintType -> String
prettyPrintRow = render . prettyPrintRowWith defaultOptions '(' ')'

typeApp :: Pattern () PrettyPrintType (PrettyPrintType, PrettyPrintType)
typeApp = mkPattern match
  where
  match (PPTypeApp f x) = Just (f, x)
  match _ = Nothing

appliedFunction :: Pattern () PrettyPrintType (PrettyPrintType, PrettyPrintType)
appliedFunction = mkPattern match
  where
  match (PPFunction arg ret) = Just (arg, ret)
  match _ = Nothing

kinded :: Pattern () PrettyPrintType (Kind (), PrettyPrintType)
kinded = mkPattern match
  where
  match (PPKindedType t k) = Just (k, t)
  match _ = Nothing

constrained :: Pattern () PrettyPrintType (PrettyPrintConstraint, PrettyPrintType)
constrained = mkPattern match
  where
  match (PPConstrainedType deps ty) = Just (deps, ty)
  match _ = Nothing

explicitParens :: Pattern () PrettyPrintType ((), PrettyPrintType)
explicitParens = mkPattern match
  where
  match (PPParensInType ty) = Just ((), ty)
  match _ = Nothing

matchTypeAtom :: TypeRenderOptions -> Pattern () PrettyPrintType Box
matchTypeAtom tro@TypeRenderOptions{troSuggesting = suggesting} =
    typeLiterals <+> fmap ((`before` (text ")")) . (text "(" <>)) (matchType tro)
  where
    typeLiterals :: Pattern () PrettyPrintType Box
    typeLiterals = mkPattern match where
      match (PPTypeWildcard name) = Just $ maybe (text "_") (text . ('?' :) . T.unpack) name
      match (PPTypeVar var) = Just $ text $ T.unpack var
      match (PPTypeLevelString s) = Just $ text $ T.unpack $ prettyPrintString s
      match (PPRecord row) = Just $ prettyPrintRowWith tro '{' '}' row
      match (PPTypeConstructor ctor) = Just $ text $ T.unpack $ runProperName $ disqualify ctor
      match (PPTUnknown u)
        | suggesting = Just $ text "_"
        | otherwise = Just $ text $ 't' : show u
      match (PPSkolem name s)
        | suggesting =  Just $ text $ T.unpack name
        | otherwise = Just $ text $ T.unpack name ++ show s
      match PPREmpty = Just $ text "()"
      match row@PPRCons{} = Just $ prettyPrintRowWith tro '(' ')' row
      match (PPBinaryNoParensType op l r) =
        Just $ typeAsBox' l <> text " " <> typeAsBox' op <> text " " <> typeAsBox' r
      match (PPTypeOp op) = Just $ text $ T.unpack $ showQualified runOpName op
      match _ = Nothing

matchType :: TypeRenderOptions -> Pattern () PrettyPrintType Box
matchType tro = buildPrettyPrinter operators (matchTypeAtom tro) where
  operators :: OperatorTable () PrettyPrintType Box
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

forall_ :: Pattern () PrettyPrintType ([String], PrettyPrintType)
forall_ = mkPattern match
  where
  match (PPForAll idents ty) = Just (map T.unpack idents, ty)
  match _ = Nothing

typeAtomAsBox' :: PrettyPrintType -> Box
typeAtomAsBox'
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern (matchTypeAtom defaultOptions) ()

typeAtomAsBox :: Type a -> Box
typeAtomAsBox = typeAtomAsBox' . convertPrettyPrintType

-- | Generate a pretty-printed string representing a Type, as it should appear inside parentheses
prettyPrintTypeAtom :: Type a -> String
prettyPrintTypeAtom = render . typeAtomAsBox

typeAsBox' :: PrettyPrintType -> Box
typeAsBox' = typeAsBoxImpl defaultOptions

typeAsBox :: Type a -> Box
typeAsBox = typeAsBox' . convertPrettyPrintType

suggestedTypeAsBox :: PrettyPrintType -> Box
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

typeAsBoxImpl :: TypeRenderOptions -> PrettyPrintType -> Box
typeAsBoxImpl tro
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern (matchType tro) ()

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
prettyPrintType' tro = render . typeAsBoxImpl tro . convertPrettyPrintType

prettyPrintLabel :: Label -> Text
prettyPrintLabel (Label s) =
  case decodeString s of
    Just s' | not (objectKeyRequiresQuoting s') ->
      s'
    _ ->
      prettyPrintString s

prettyPrintObjectKey :: PSString -> Text
prettyPrintObjectKey = prettyPrintLabel . Label
