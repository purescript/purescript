-- |
-- Pretty printer for Types
--
module Language.PureScript.Pretty.Types
  ( PrettyPrintType(..)
  , PrettyPrintConstraint
  , convertPrettyPrintType
  , typeAsBox
  , typeDiffAsBox
  , prettyPrintType
  , prettyPrintTypeWithUnicode
  , prettyPrintSuggestedType
  , typeAtomAsBox
  , prettyPrintTypeAtom
  , prettyPrintLabel
  , prettyPrintObjectKey
  ) where

import Prelude hiding ((<>))

import Control.Arrow ((<+>))
import Control.Lens (_2, (%~))
import Control.PatternArrows as PA

import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import Data.Text qualified as T

import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (tyFunction, tyRecord)
import Language.PureScript.Names (OpName(..), OpNameType(..), ProperName(..), ProperNameType(..), Qualified, coerceProperName, disqualify, showQualified)
import Language.PureScript.Pretty.Common (before, objectKeyRequiresQuoting)
import Language.PureScript.Types (Constraint(..), pattern REmptyKinded, RowListItem(..), Type(..), TypeVarVisibility(..), WildcardData(..), eqType, rowToSortedList, typeVarVisibilityPrefix)
import Language.PureScript.PSString (PSString, prettyPrintString, decodeString)
import Language.PureScript.Label (Label(..))

import Text.PrettyPrint.Boxes (Box(..), hcat, hsep, left, moveRight, nullBox, render, text, top, vcat, (<>))

data PrettyPrintType
  = PPTUnknown Int
  | PPTypeVar Text (Maybe Text)
  | PPTypeLevelString PSString
  | PPTypeLevelInt Integer
  | PPTypeWildcard (Maybe Text)
  | PPTypeConstructor (Qualified (ProperName 'TypeName))
  | PPTypeOp (Qualified (OpName 'TypeOpName))
  | PPSkolem Text Int
  | PPTypeApp PrettyPrintType PrettyPrintType
  | PPKindArg PrettyPrintType
  | PPConstrainedType PrettyPrintConstraint PrettyPrintType
  | PPKindedType PrettyPrintType PrettyPrintType
  | PPBinaryNoParensType PrettyPrintType PrettyPrintType PrettyPrintType
  | PPParensInType PrettyPrintType
  | PPForAll [(TypeVarVisibility, Text, Maybe PrettyPrintType)] PrettyPrintType
  | PPFunction PrettyPrintType PrettyPrintType
  | PPRecord [(Label, PrettyPrintType)] (Maybe PrettyPrintType)
  | PPRow [(Label, PrettyPrintType)] (Maybe PrettyPrintType)
  | PPTruncated

type PrettyPrintConstraint = (Qualified (ProperName 'ClassName), [PrettyPrintType], [PrettyPrintType])

convertPrettyPrintType :: Int -> Type a -> PrettyPrintType
convertPrettyPrintType = go
  where
  go _ (TUnknown _ n) = PPTUnknown n
  go _ (TypeVar _ t) = PPTypeVar t Nothing
  go _ (TypeLevelString _ s) = PPTypeLevelString s
  go _ (TypeLevelInt _ n) = PPTypeLevelInt n
  go _ (TypeWildcard _ (HoleWildcard n)) = PPTypeWildcard (Just n)
  go _ (TypeWildcard _ _) = PPTypeWildcard Nothing
  go _ (TypeConstructor _ c) = PPTypeConstructor c
  go _ (TypeOp _ o) = PPTypeOp o
  go _ (Skolem _ t _ n _) = PPSkolem t n
  go _ (REmpty _) = PPRow [] Nothing
  -- Guard the remaining "complex" type atoms on the current depth value. The
  -- prior  constructors can all be printed simply so it's not really helpful to
  -- truncate them.
  go d _ | d < 0 = PPTruncated
  go d (ConstrainedType _ (Constraint _ cls kargs args _) ty) = PPConstrainedType (cls, go (d-1) <$> kargs, go (d-1) <$> args) (go d ty)
  go d (KindedType _ ty k) = PPKindedType (go (d-1) ty) (go (d-1) k)
  go d (BinaryNoParensType _ ty1 ty2 ty3) = PPBinaryNoParensType (go (d-1) ty1) (go (d-1) ty2) (go (d-1) ty3)
  go d (ParensInType _ ty) = PPParensInType (go (d-1) ty)
  go d ty@RCons{} = uncurry PPRow (goRow d ty)
  go d (ForAll _ vis v mbK ty _) = goForAll d [(vis, v, fmap (go (d-1)) mbK)] ty
  go d (TypeApp _ a b) = goTypeApp d a b
  go d (KindApp _ a b) = PPTypeApp (go (d-1) a) (PPKindArg (go (d-1) b))

  goForAll d vs (ForAll _ vis v mbK ty _) = goForAll d ((vis, v, fmap (go (d-1)) mbK) : vs) ty
  goForAll d vs ty = PPForAll (reverse vs) (go (d-1) ty)

  goRow d ty =
    let (items, tail_) = rowToSortedList ty
    in ( map (\item -> (rowListLabel item, go (d-1) (rowListType item))) items
       , case tail_ of
           REmptyKinded _ _ -> Nothing
           _ -> Just (go (d-1) tail_)
       )

  goTypeApp d (TypeApp _ f a) b
    | eqType f tyFunction = PPFunction (go (d-1) a) (go (d-1) b)
    | otherwise = PPTypeApp (goTypeApp d f a) (go (d-1) b)
  goTypeApp d o ty@RCons{}
    | eqType o tyRecord = uncurry PPRecord (goRow d ty)
  goTypeApp d a b = PPTypeApp (go (d-1) a) (go (d-1) b)

-- TODO(Christoph): get rid of T.unpack s

constraintsAsBox :: TypeRenderOptions -> PrettyPrintConstraint -> Box -> Box
constraintsAsBox tro con ty =
    constraintAsBox con `before` (" " <> text doubleRightArrow <> " " <> ty)
  where
    doubleRightArrow = if troUnicode tro then "⇒" else "=>"

constraintAsBox :: PrettyPrintConstraint -> Box
constraintAsBox (pn, ks, tys) = typeAsBox' (foldl PPTypeApp (foldl (\a b -> PPTypeApp a (PPKindArg b)) (PPTypeConstructor (fmap coerceProperName pn)) ks) tys)

-- |
-- Generate a pretty-printed string representing a Row
--
prettyPrintRowWith :: TypeRenderOptions -> Char -> Char -> [(Label, PrettyPrintType)] -> Maybe PrettyPrintType -> Box
prettyPrintRowWith tro open close labels rest =
  case (labels, rest) of
    ([], Nothing) ->
      if troRowAsDiff tro then text [ open, ' ' ] <> text "..." <> text [ ' ', close ] else text [ open, close ]
    ([], Just _) ->
      text [ open, ' ' ] <> tailToPs rest <> text [ ' ', close ]
    _ ->
      vcat left $
        zipWith (\(nm, ty) i -> nameAndTypeToPs (if i == 0 then open else ',') nm ty) labels [0 :: Int ..] ++
        catMaybes [ rowDiff, pure $ tailToPs rest, pure $ text [close] ]

  where
  nameAndTypeToPs :: Char -> Label -> PrettyPrintType -> Box
  nameAndTypeToPs start name ty = text (start : ' ' : T.unpack (prettyPrintLabel name) ++ " " ++ doubleColon ++ " ") <> typeAsBox' ty

  doubleColon = if troUnicode tro then "∷" else "::"

  rowDiff = if troRowAsDiff tro then Just (text "...") else Nothing

  tailToPs :: Maybe PrettyPrintType -> Box
  tailToPs Nothing = nullBox
  tailToPs (Just other) = text "| " <> typeAsBox' other

typeApp :: Pattern () PrettyPrintType (PrettyPrintType, PrettyPrintType)
typeApp = mkPattern match
  where
  match (PPTypeApp f x) = Just (f, x)
  match _ = Nothing

kindArg :: Pattern () PrettyPrintType ((), PrettyPrintType)
kindArg = mkPattern match
  where
  match (PPKindArg ty) = Just ((), ty)
  match _ = Nothing

appliedFunction :: Pattern () PrettyPrintType (PrettyPrintType, PrettyPrintType)
appliedFunction = mkPattern match
  where
  match (PPFunction arg ret) = Just (arg, ret)
  match _ = Nothing

kinded :: Pattern () PrettyPrintType (PrettyPrintType, PrettyPrintType)
kinded = mkPattern match
  where
  match (PPKindedType t k) = Just (t, k)
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
    typeLiterals <+> fmap ((`before` text ")") . (text "(" <>)) (matchType tro)
  where
    typeLiterals :: Pattern () PrettyPrintType Box
    typeLiterals = mkPattern match where
      match (PPTypeWildcard name) = Just $ text $ maybe "_" (('?' :) . T.unpack) name
      match (PPTypeVar var _) = Just $ text $ T.unpack var
      match (PPTypeLevelString s) = Just $ text $ T.unpack $ prettyPrintString s
      match (PPTypeLevelInt n) = Just $ text $ show n
      match (PPTypeConstructor ctor) = Just $ text $ T.unpack $ runProperName $ disqualify ctor
      match (PPTUnknown u)
        | suggesting = Just $ text "_"
        | otherwise = Just $ text $ 't' : show u
      match (PPSkolem name s)
        | suggesting =  Just $ text $ T.unpack name
        | otherwise = Just $ text $ T.unpack name ++ show s
      match (PPRecord labels tail_) = Just $ prettyPrintRowWith tro '{' '}' labels tail_
      match (PPRow labels tail_) = Just $ prettyPrintRowWith tro '(' ')' labels tail_
      match (PPBinaryNoParensType op l r) =
        Just $ typeAsBox' l <> text " " <> typeAsBox' op <> text " " <> typeAsBox' r
      match (PPTypeOp op) = Just $ text $ T.unpack $ showQualified runOpName op
      match PPTruncated = Just $ text "..."
      match _ = Nothing

matchType :: TypeRenderOptions -> Pattern () PrettyPrintType Box
matchType tro = buildPrettyPrinter operators (matchTypeAtom tro) where
  operators :: OperatorTable () PrettyPrintType Box
  operators =
    OperatorTable [ [ Wrap kindArg $ \_ ty -> text "@" <> ty ]
                  , [ AssocL typeApp $ \f x -> keepSingleLinesOr (moveRight 2) f x ]
                  , [ AssocR appliedFunction $ \arg ret -> keepSingleLinesOr id arg (text rightArrow <> " " <> ret) ]
                  , [ Wrap constrained $ \deps ty -> constraintsAsBox tro deps ty ]
                  , [ Wrap forall_ $ \idents ty -> keepSingleLinesOr (moveRight 2) (hsep 1 top (text forall' : fmap printMbKindedType idents) <> text ".") ty ]
                  , [ Wrap kinded $ \ty k -> keepSingleLinesOr (moveRight 2) (typeAsBox' ty) (text (doubleColon ++ " ") <> k) ]
                  , [ Wrap explicitParens $ \_ ty -> ty ]
                  ]

  rightArrow = if troUnicode tro then "→" else "->"
  forall' = if troUnicode tro then "∀" else "forall"
  doubleColon = if troUnicode tro then "∷" else "::"

  printMbKindedType (vis, v, Nothing) = text (T.unpack $ typeVarVisibilityPrefix vis) <> text v
  printMbKindedType (vis, v, Just k) = text ("(" ++ T.unpack (typeVarVisibilityPrefix vis) ++ v ++ " " ++ doubleColon ++ " ") <> typeAsBox' k <> text ")"

  -- If both boxes span a single line, keep them on the same line, or else
  -- use the specified function to modify the second box, then combine vertically.
  keepSingleLinesOr :: (Box -> Box) -> Box -> Box -> Box
  keepSingleLinesOr f b1 b2
    | rows b1 > 1 || rows b2 > 1 = vcat left [ b1, f b2 ]
    | otherwise = hcat top [ b1, text " ", b2]

forall_ :: Pattern () PrettyPrintType ([(TypeVarVisibility, String, Maybe PrettyPrintType)], PrettyPrintType)
forall_ = mkPattern match
  where
  match (PPForAll idents ty) = Just ((_2 %~ T.unpack) <$> idents, ty)
  match _ = Nothing

typeAtomAsBox' :: PrettyPrintType -> Box
typeAtomAsBox'
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern_ (matchTypeAtom defaultOptions) ()

typeAtomAsBox :: Int -> Type a -> Box
typeAtomAsBox maxDepth = typeAtomAsBox' . convertPrettyPrintType maxDepth

-- | Generate a pretty-printed string representing a Type, as it should appear inside parentheses
prettyPrintTypeAtom :: Int -> Type a -> String
prettyPrintTypeAtom maxDepth = render . typeAtomAsBox maxDepth

typeAsBox' :: PrettyPrintType -> Box
typeAsBox' = typeAsBoxImpl defaultOptions

typeAsBox :: Int -> Type a -> Box
typeAsBox maxDepth = typeAsBox' . convertPrettyPrintType maxDepth

typeDiffAsBox' :: PrettyPrintType -> Box
typeDiffAsBox' = typeAsBoxImpl diffOptions

typeDiffAsBox :: Int -> Type a -> Box
typeDiffAsBox maxDepth = typeDiffAsBox' . convertPrettyPrintType maxDepth

data TypeRenderOptions = TypeRenderOptions
  { troSuggesting :: Bool
  , troUnicode :: Bool
  , troRowAsDiff :: Bool
  }

suggestingOptions :: TypeRenderOptions
suggestingOptions = TypeRenderOptions True False False

defaultOptions :: TypeRenderOptions
defaultOptions = TypeRenderOptions False False False

diffOptions :: TypeRenderOptions
diffOptions = TypeRenderOptions False False True

unicodeOptions :: TypeRenderOptions
unicodeOptions = TypeRenderOptions False True False

typeAsBoxImpl :: TypeRenderOptions -> PrettyPrintType -> Box
typeAsBoxImpl tro
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern_ (matchType tro) ()

-- | Generate a pretty-printed string representing a 'Type'
prettyPrintType :: Int -> Type a -> String
prettyPrintType = flip prettyPrintType' defaultOptions

-- | Generate a pretty-printed string representing a 'Type' using unicode
-- symbols where applicable
prettyPrintTypeWithUnicode :: Int -> Type a -> String
prettyPrintTypeWithUnicode = flip prettyPrintType' unicodeOptions

-- | Generate a pretty-printed string representing a suggested 'Type'
prettyPrintSuggestedType :: Type a -> String
prettyPrintSuggestedType = prettyPrintType' maxBound suggestingOptions

prettyPrintType' :: Int -> TypeRenderOptions -> Type a -> String
prettyPrintType' maxDepth tro = render . typeAsBoxImpl tro . convertPrettyPrintType maxDepth

prettyPrintLabel :: Label -> Text
prettyPrintLabel (Label s) =
  case decodeString s of
    Just s' | not (objectKeyRequiresQuoting s') ->
      s'
    _ ->
      prettyPrintString s

prettyPrintObjectKey :: PSString -> Text
prettyPrintObjectKey = prettyPrintLabel . Label
