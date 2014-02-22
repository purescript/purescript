-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Values
-- Copyright   :  Kinds.hs(c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Pretty printer for values
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.Values (
    prettyPrintValue,
    prettyPrintBinder
) where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Control.Arrow ((<+>))
import Control.PatternArrows

import Language.PureScript.Types
import Language.PureScript.Values
import Language.PureScript.Pretty.Common
import Language.PureScript.Pretty.Types

literals :: Pattern () Value String
literals = mkPattern match
  where
  match (NumericLiteral n) = Just $ either show show n
  match (StringLiteral s) = Just $ show s
  match (BooleanLiteral True) = Just "true"
  match (BooleanLiteral False) = Just "false"
  match (ArrayLiteral xs) = Just $ "[" ++ intercalate ", " (map prettyPrintValue xs) ++ "]"
  match (ObjectLiteral ps) = Just $ "{" ++ intercalate ", " (map (uncurry prettyPrintObjectProperty) ps) ++ "}"
  match (Constructor name) = Just $ show name
  match (Case values binders) = Just $ "case " ++ unwords (map prettyPrintValue values) ++
    " of { " ++ intercalate " ; " (map prettyPrintCaseAlternative binders) ++ " }"
  match (Var ident) = Just $ show ident
  match (Do els) = Just $ " do { " ++ intercalate "; " (map prettyPrintDoNotationElement els) ++ " }"
  match (TypeClassDictionary _ _) = error "Type class dictionary was not replaced"
  match _ = Nothing

prettyPrintCaseAlternative :: ([Binder], Maybe Guard, Value) -> String
prettyPrintCaseAlternative (binders, grd, val) = "(" ++ intercalate ", " (map prettyPrintBinder binders) ++ ") " ++
  maybe "" (("| " ++) . prettyPrintValue) grd ++ " -> " ++ prettyPrintValue val

ifThenElse :: Pattern () Value ((Value, Value), Value)
ifThenElse = mkPattern match
  where
  match (IfThenElse cond th el) = Just ((th, el), cond)
  match _ = Nothing

accessor :: Pattern () Value (String, Value)
accessor = mkPattern match
  where
  match (Accessor prop val) = Just (prop, val)
  match _ = Nothing

objectUpdate :: Pattern () Value ([String], Value)
objectUpdate = mkPattern match
  where
  match (ObjectUpdate o ps) = Just (flip map ps $ \(key, val) -> key ++ " = " ++ prettyPrintValue val, o)
  match _ = Nothing

app :: Pattern () Value (String, Value)
app = mkPattern match
  where
  match (App val arg) = Just (prettyPrintValue arg, val)
  match _ = Nothing

lam :: Pattern () Value (String, Value)
lam = mkPattern match
  where
  match (Abs (Left arg) val) = Just (show arg, val)
  match _ = Nothing

typed :: Pattern () Value (Type, Value)
typed = mkPattern match
  where
  match (TypedValue _ val ty) = Just (ty, val)
  match _ = Nothing

prettyPrintDoNotationElement :: DoNotationElement -> String
prettyPrintDoNotationElement (DoNotationValue val) = prettyPrintValue val
prettyPrintDoNotationElement (DoNotationBind binder val) = prettyPrintBinder binder ++ " <- " ++ prettyPrintValue val
prettyPrintDoNotationElement (DoNotationLet binder val) = "let " ++ prettyPrintBinder binder ++ " = " ++ prettyPrintValue val

-- |
-- Generate a pretty-printed string representing a Value
--
prettyPrintValue :: Value -> String
prettyPrintValue = fromMaybe (error "Incomplete pattern") . pattern matchValue ()
  where
  matchValue :: Pattern () Value String
  matchValue = buildPrettyPrinter operators (literals <+> fmap parens matchValue)
  operators :: OperatorTable () Value String
  operators =
    OperatorTable [ [ Wrap accessor $ \prop val -> val ++ "." ++ prop ]
                  , [ Wrap objectUpdate $ \ps val -> val ++ "{ " ++ intercalate ", " ps ++ " }" ]
                  , [ Wrap app $ \arg val -> val ++ "(" ++ arg ++ ")" ]
                  , [ Split lam $ \arg val -> "\\" ++ arg ++ " -> " ++ prettyPrintValue val ]
                  , [ Wrap ifThenElse $ \(th, el) cond -> cond ++ " ? " ++ prettyPrintValue th ++ " : " ++ prettyPrintValue el ]
                  , [ Wrap typed $ \ty val -> val ++ " :: " ++ prettyPrintType ty ]
                  ]

prettyPrintBinderAtom :: Pattern () Binder String
prettyPrintBinderAtom = mkPattern match
  where
  match :: Binder -> Maybe String
  match NullBinder = Just "_"
  match (StringBinder str) = Just $ show str
  match (NumberBinder num) = Just $ either show show num
  match (BooleanBinder True) = Just "true"
  match (BooleanBinder False) = Just "false"
  match (VarBinder ident) = Just $ show ident
  match (ConstructorBinder ctor args) = Just $ show ctor ++ " " ++ unwords (map (parens . prettyPrintBinder) args)
  match (ObjectBinder bs) = Just $ "{ " ++ intercalate ", " (map (uncurry prettyPrintObjectPropertyBinder) bs) ++ " }"
  match (ArrayBinder bs) = Just $ "[ " ++ intercalate ", " (map prettyPrintBinder bs) ++ " ]"
  match (NamedBinder ident binder) = Just $ show ident ++ "@" ++ prettyPrintBinder binder
  match _ = Nothing

-- |
-- Generate a pretty-printed string representing a Binder
--
prettyPrintBinder :: Binder -> String
prettyPrintBinder = fromMaybe (error "Incomplete pattern") . pattern matchBinder ()
  where
  matchBinder :: Pattern () Binder String
  matchBinder = buildPrettyPrinter operators (prettyPrintBinderAtom <+> fmap parens matchBinder)
  operators :: OperatorTable () Binder String
  operators =
    OperatorTable [ [ AssocR matchConsBinder (\b1 b2 -> b1 ++ " : " ++ b2) ] ]

matchConsBinder :: Pattern () Binder (Binder, Binder)
matchConsBinder = mkPattern match'
  where
  match' (ConsBinder b1 b2) = Just (b1, b2)
  match' _ = Nothing

prettyPrintObjectPropertyBinder :: String -> Binder -> String
prettyPrintObjectPropertyBinder key binder = key ++ ": " ++ prettyPrintBinder binder

prettyPrintObjectProperty :: String -> Value -> String
prettyPrintObjectProperty key value = key ++ ": " ++ prettyPrintValue value
