-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Types
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Pretty printer for Types
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.Types (
    typeAsBox,
    prettyPrintType,
    typeAtomAsBox,
    prettyPrintTypeAtom,
    prettyPrintRowWith,
    prettyPrintRow
) where

import Data.Maybe (fromMaybe)

import Control.Arrow ((<+>))
import Control.PatternArrows

import Language.PureScript.Crash
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Kinds
import Language.PureScript.Pretty.Common
import Language.PureScript.Pretty.Kinds
import Language.PureScript.Environment

import Text.PrettyPrint.Boxes hiding ((<+>))

typeLiterals :: Pattern () Type Box
typeLiterals = mkPattern match
  where
  match TypeWildcard = Just $ text "_"
  match (TypeVar var) = Just $ text var
  match (PrettyPrintObject row) = Just $ prettyPrintRowWith '{' '}' row
  match (TypeConstructor ctor) = Just $ text $ runProperName $ disqualify ctor
  match (TUnknown u) = Just $ text $ '_' : show u
  match (Skolem name s _) = Just $ text $ name ++ show s
  match REmpty = Just $ text "()"
  match row@RCons{} = Just $ prettyPrintRowWith '(' ')' row
  match _ = Nothing

constraintsAsBox :: [(Qualified ProperName, [Type])] -> Box -> Box
constraintsAsBox [(pn, tys)] ty = text "(" <> constraintAsBox pn tys <> text ") => " <> ty
constraintsAsBox xs ty = vcat left (zipWith (\i (pn, tys) -> text (if i == 0 then "( " else ", ") <> constraintAsBox pn tys) [0 :: Int ..] xs) `before` (text ") => " <> ty)

constraintAsBox :: Qualified ProperName -> [Type] -> Box
constraintAsBox pn tys = hsep 1 left (text (runProperName (disqualify pn)) : map typeAtomAsBox tys)

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
  toList tys r = (tys, r)

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
  convert (TypeApp o r) | o == tyObject = PrettyPrintObject r
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

matchTypeAtom :: Pattern () Type Box
matchTypeAtom = typeLiterals <+> fmap ((`before` text ")") . (text "(" <>)) matchType

matchType :: Pattern () Type Box
matchType = buildPrettyPrinter operators matchTypeAtom
  where
  operators :: OperatorTable () Type Box
  operators =
    OperatorTable [ [ AssocL typeApp $ \f x -> f `beforeWithSpace` x ]
                  , [ AssocR appliedFunction $ \arg ret -> (arg <> text " ") `before` (text "-> " <> ret)
                    ]
                  , [ Wrap constrained $ \deps ty -> constraintsAsBox deps ty ]
                  , [ Wrap forall_ $ \idents ty -> text ("forall " ++ unwords idents ++ ". ") <> ty ]
                  , [ Wrap kinded $ \k ty -> ty `before` (text (" :: " ++ prettyPrintKind k)) ]
                  ]

forall_ :: Pattern () Type ([String], Type)
forall_ = mkPattern match
  where
  match (PrettyPrintForAll idents ty) = Just (idents, ty)
  match _ = Nothing

typeAtomAsBox :: Type -> Box
typeAtomAsBox = fromMaybe (internalError "Incomplete pattern") . pattern matchTypeAtom () . insertPlaceholders

-- | Generate a pretty-printed string representing a Type, as it should appear inside parentheses
prettyPrintTypeAtom :: Type -> String
prettyPrintTypeAtom = render . typeAtomAsBox

typeAsBox :: Type -> Box
typeAsBox = fromMaybe (internalError "Incomplete pattern") . pattern matchType () . insertPlaceholders

-- | Generate a pretty-printed string representing a Type
prettyPrintType :: Type -> String
prettyPrintType = render . typeAsBox
