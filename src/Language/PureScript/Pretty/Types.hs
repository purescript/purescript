-- |
-- Pretty printer for Types
--
module Language.PureScript.Pretty.Types
  ( PrettyPrintType(..)
  , PrettyPrintConstraint
  , convertPrettyPrintType
  , typeAsBox
  , typeDiffAsBox
  , suggestedTypeAsBox
  , prettyPrintType
  , prettyPrintTypeWithUnicode
  , prettyPrintSuggestedType
  , typeAtomAsBox
  , prettyPrintTypeAtom
  , prettyPrintLabel
  , prettyPrintObjectKey
  ) where

import Prelude.Compat hiding ((<>))

import Control.Arrow ((<+>))
import Control.PatternArrows as PA

import Data.Functor (($>))
import Data.Maybe (fromMaybe, catMaybes)
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
  | PPForAll [(Text, Maybe (Kind ()))] PrettyPrintType
  | PPFunction PrettyPrintType PrettyPrintType
  | PPRecord [(Label, PrettyPrintType)] (Maybe PrettyPrintType)
  | PPRow [(Label, PrettyPrintType)] (Maybe PrettyPrintType)
  | PPTruncated

type PrettyPrintConstraint = (Qualified (ProperName 'ClassName), [PrettyPrintType])

convertPrettyPrintType :: Int -> Type a -> PrettyPrintType
convertPrettyPrintType = go
  where
  go _ (TUnknown _ n) = PPTUnknown n
  go _ (TypeVar _ t) = PPTypeVar t
  go _ (TypeLevelString _ s) = PPTypeLevelString s
  go _ (TypeWildcard _ n) = PPTypeWildcard n
  go _ (TypeConstructor _ c) = PPTypeConstructor c
  go _ (TypeOp _ o) = PPTypeOp o
  go _ (Skolem _ t n _) = PPSkolem t n
  go _ (REmpty _) = PPRow [] Nothing
  -- Guard the remaining "complex" type atoms on the current depth value. The
  -- prior  constructors can all be printed simply so it's not really helpful to
  -- truncate them.
  go d _ | d < 0 = PPTruncated
  go d (ConstrainedType _ (Constraint _ cls args _) ty) = PPConstrainedType (cls, go (d-1) <$> args) (go d ty)
  go d (KindedType _ ty k) = PPKindedType (go (d-1) ty) (k $> ())
  go d (BinaryNoParensType _ ty1 ty2 ty3) = PPBinaryNoParensType (go (d-1) ty1) (go (d-1) ty2) (go (d-1) ty3)
  go d (ParensInType _ ty) = PPParensInType (go (d-1) ty)
  go d ty@RCons{} = uncurry PPRow (goRow d ty)
  go d (ForAll _ v mbK ty _) = goForAll d [(v, fmap ($> ()) mbK)] ty
  go d (TypeApp _ a b) = goTypeApp d a b

  goForAll d vs (ForAll _ v mbK ty _) = goForAll d ((v, fmap ($> ()) mbK) : vs) ty
  goForAll d vs ty = PPForAll vs (go (d-1) ty)

  goRow d ty =
    let (items, tail_) = rowToSortedList ty
    in ( map (\item -> (rowListLabel item, go (d-1) (rowListType item))) items
       , case tail_ of
           REmpty _ -> Nothing
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
constraintAsBox (pn, tys) = typeAsBox' (foldl PPTypeApp (PPTypeConstructor (fmap coerceProperName pn)) tys)

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
    OperatorTable [ [ AssocL typeApp $ \f x -> keepSingleLinesOr (moveRight 2) f x ]
                  , [ AssocR appliedFunction $ \arg ret -> keepSingleLinesOr id arg (text rightArrow <> " " <> ret) ]
                  , [ Wrap constrained $ \deps ty -> constraintsAsBox tro deps ty ]
                  , [ Wrap forall_ $ \idents ty -> keepSingleLinesOr (moveRight 2) (text (forall' ++ " " ++ unwords (fmap printMbKindedType idents) ++ ".")) ty ]
                  , [ Wrap kinded $ \k ty -> keepSingleLinesOr (moveRight 2) ty (text (doubleColon ++ " " ++ T.unpack (prettyPrintKind k))) ]
                  , [ Wrap explicitParens $ \_ ty -> ty ]
                  ]

  rightArrow = if troUnicode tro then "→" else "->"
  forall' = if troUnicode tro then "∀" else "forall"
  doubleColon = if troUnicode tro then "∷" else "::"
  printMbKindedType (v, mbK) = maybe v (\k -> unwords ["(" ++ v, doubleColon, T.unpack (prettyPrintKind k) ++ ")"]) mbK

  -- If both boxes span a single line, keep them on the same line, or else
  -- use the specified function to modify the second box, then combine vertically.
  keepSingleLinesOr :: (Box -> Box) -> Box -> Box -> Box
  keepSingleLinesOr f b1 b2
    | rows b1 > 1 || rows b2 > 1 = vcat left [ b1, f b2 ]
    | otherwise = hcat top [ b1, text " ", b2]

forall_ :: Pattern () PrettyPrintType ([(String, Maybe (Kind ()))], PrettyPrintType)
forall_ = mkPattern match
  where
  match (PPForAll idents ty) = Just (map (\(v, mbK) -> (T.unpack v, mbK)) idents, ty)
  match _ = Nothing

typeAtomAsBox' :: PrettyPrintType -> Box
typeAtomAsBox'
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern (matchTypeAtom defaultOptions) ()

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

suggestedTypeAsBox :: PrettyPrintType -> Box
suggestedTypeAsBox = typeAsBoxImpl suggestingOptions

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
  . PA.pattern (matchType tro) ()

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
