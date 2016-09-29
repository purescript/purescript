-- |
-- Pretty printer for Types
--
module Language.PureScript.Pretty.Types
  ( typeAsBox
  , prettyPrintType
  , typeAtomAsBox
  , prettyPrintTypeAtom
  , prettyPrintRowWith
  , prettyPrintRow
  ) where

import Prelude.Compat

import Control.Arrow ((<+>))
import Control.PatternArrows as PA

import Data.Maybe (fromMaybe)

import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Kinds
import Language.PureScript.Names
import Language.PureScript.Pretty.Common
import Language.PureScript.Pretty.Kinds
import Language.PureScript.Types

import Text.PrettyPrint.Boxes hiding ((<+>))

typeLiterals :: Pattern () Type Box
typeLiterals = mkPattern match
  where
  match TypeWildcard{} = Just $ text "_"
  match (TypeVar var) = Just $ text var
  match (TypeLevelString s) = Just . text $ show s
  match (PrettyPrintObject row) = Just $ prettyPrintRowWith '{' '}' row
  match (TypeConstructor ctor) = Just $ text $ runProperName $ disqualify ctor
  match (TUnknown u) = Just $ text $ 't' : show u
  match (Skolem name s _ _) = Just $ text $ name ++ show s
  match REmpty = Just $ text "()"
  match row@RCons{} = Just $ prettyPrintRowWith '(' ')' row
  match (BinaryNoParensType op l r) =
    Just $ typeAsBox l <> text " " <> typeAsBox op <> text " " <> typeAsBox r
  match (TypeOp op) = Just $ text $ showQualified runOpName op
  match _ = Nothing

constraintsAsBox :: [Constraint] -> Box -> Box
constraintsAsBox [con] ty = text "(" <> constraintAsBox con `before` (text ") => " <> ty)
constraintsAsBox xs ty = vcat left (zipWith (\i con -> text (if i == 0 then "( " else ", ") <> constraintAsBox con) [0 :: Int ..] xs) `before` (text ") => " <> ty)

constraintAsBox :: Constraint -> Box
constraintAsBox (Constraint pn tys _) = typeAsBox (foldl TypeApp (TypeConstructor (fmap coerceProperName pn)) tys)

-- |
-- Generate a pretty-printed string representing a Row
--
prettyPrintRowWith :: Char -> Char -> Type -> Box
prettyPrintRowWith open close = uncurry listToBox . toList []
  where
  nameAndTypeToPs :: Char -> String -> Type -> Box
  nameAndTypeToPs start name ty = text (start : ' ' : prettyPrintObjectKey name ++ " :: ") <> typeAsBox ty

  tailToPs :: Type -> Box
  tailToPs REmpty = nullBox
  tailToPs other = text "| " <> typeAsBox other

  listToBox :: [(String, Type)] -> Type -> Box
  listToBox [] REmpty = text [open, close]
  listToBox [] rest = text [ open, ' ' ] <> tailToPs rest <> text [ ' ', close ]
  listToBox ts rest = vcat left $
    zipWith (\(nm, ty) i -> nameAndTypeToPs (if i == 0 then open else ',') nm ty) ts [0 :: Int ..] ++
    [ tailToPs rest, text [close] ]

  toList :: [(String, Type)] -> Type -> ([(String, Type)], Type)
  toList tys (RCons name ty row) = toList ((name, ty):tys) row
  toList tys r = (reverse tys, r)

prettyPrintRow :: Type -> String
prettyPrintRow = render . prettyPrintRowWith '(' ')'

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

constrained :: Pattern () Type ([Constraint], Type)
constrained = mkPattern match
  where
  match (ConstrainedType deps ty) = Just (deps, ty)
  match _ = Nothing

explicitParens :: Pattern () Type ((), Type)
explicitParens = mkPattern match
  where
  match (ParensInType ty) = Just ((), ty)
  match _ = Nothing

matchTypeAtom :: Pattern () Type Box
matchTypeAtom = typeLiterals <+> fmap ((`before` (text ")")) . (text "(" <>)) matchType

matchType :: Pattern () Type Box
matchType = buildPrettyPrinter operators matchTypeAtom
  where
  operators :: OperatorTable () Type Box
  operators =
    OperatorTable [ [ AssocL typeApp $ \f x -> keepSingleLinesOr (moveRight 2) f x ]
                  , [ AssocR appliedFunction $ \arg ret -> keepSingleLinesOr id arg (text "-> " <> ret) ]
                  , [ Wrap constrained $ \deps ty -> constraintsAsBox deps ty ]
                  , [ Wrap forall_ $ \idents ty -> keepSingleLinesOr (moveRight 2) (text ("forall " ++ unwords idents ++ ".")) ty ]
                  , [ Wrap kinded $ \k ty -> keepSingleLinesOr (moveRight 2) ty (text (":: " ++ prettyPrintKind k)) ]
                  , [ Wrap explicitParens $ \_ ty -> ty ]
                  ]

  -- If both boxes span a single line, keep them on the same line, or else
  -- use the specified function to modify the second box, then combine vertically.
  keepSingleLinesOr :: (Box -> Box) -> Box -> Box -> Box
  keepSingleLinesOr f b1 b2
    | rows b1 > 1 || rows b2 > 1 = vcat left [ b1, f b2 ]
    | otherwise = hcat top [ b1, text " ", b2]

forall_ :: Pattern () Type ([String], Type)
forall_ = mkPattern match
  where
  match (PrettyPrintForAll idents ty) = Just (idents, ty)
  match _ = Nothing

typeAtomAsBox :: Type -> Box
typeAtomAsBox
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchTypeAtom ()
  . insertPlaceholders

-- | Generate a pretty-printed string representing a Type, as it should appear inside parentheses
prettyPrintTypeAtom :: Type -> String
prettyPrintTypeAtom = render . typeAtomAsBox

typeAsBox :: Type -> Box
typeAsBox
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern matchType ()
  . insertPlaceholders

-- | Generate a pretty-printed string representing a Type
prettyPrintType :: Type -> String
prettyPrintType = render . typeAsBox
