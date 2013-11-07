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
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.Values (
    prettyPrintValue,
    prettyPrintBinder
) where

import Data.Char
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Control.Arrow as A
import Control.Arrow ((<+>))
import Control.Applicative

import Language.PureScript.Types
import Language.PureScript.Values
import Language.PureScript.Names
import Language.PureScript.Pretty.Common

literals :: Pattern Value String
literals = Pattern $ A.Kleisli match
  where
  match (NumericLiteral n) = Just $ either show show n
  match (StringLiteral s) = Just $ show s
  match (BooleanLiteral True) = Just "true"
  match (BooleanLiteral False) = Just "false"
  match (ArrayLiteral xs) = Just $ "[" ++ intercalate ", " (map prettyPrintValue xs) ++ "]"
  match (ObjectLiteral ps) = Just $ "{" ++ intercalate ", " (map (uncurry prettyPrintObjectProperty) ps) ++ "}"
  match (Constructor name) = Just name
  match (Block sts) = Just $ "do { " ++ intercalate " ; " (map prettyPrintStatement sts) ++ " }"
  match (Case value binders) = Just $ "case " ++ prettyPrintValue value ++ " of { " ++ intercalate " ; " (map (uncurry prettyPrintCaseAlternative) binders) ++ " }"
  match (Var ident) = Just $ show ident
  match _ = Nothing

prettyPrintCaseAlternative :: Binder -> Value -> String
prettyPrintCaseAlternative binder val = prettyPrintBinder binder ++ " -> " ++ prettyPrintValue val

ifThenElse :: Pattern Value ((Value, Value), Value)
ifThenElse = Pattern $ A.Kleisli match
  where
  match (IfThenElse cond th el) = Just ((th, el), cond)
  match _ = Nothing

accessor :: Pattern Value (String, Value)
accessor = Pattern $ A.Kleisli match
  where
  match (Accessor prop val) = Just (prop, val)
  match _ = Nothing

indexer :: Pattern Value (Value, Value)
indexer = Pattern $ A.Kleisli match
  where
  match (Indexer index val) = Just (index, val)
  match _ = Nothing

objectUpdate :: Pattern Value ([String], Value)
objectUpdate = Pattern $ A.Kleisli match
  where
  match (ObjectUpdate o ps) = Just (flip map ps $ \(key, val) -> key ++ " = " ++ prettyPrintValue val, o)
  match _ = Nothing

app :: Pattern Value (String, Value)
app = Pattern $ A.Kleisli match
  where
  match (App val args) = Just (intercalate "," (map prettyPrintValue args), val)
  match _ = Nothing

lam :: Pattern Value ([String], Value)
lam = Pattern $ A.Kleisli match
  where
  match (Abs args val) = Just (map show args, val)
  match _ = Nothing

unary :: UnaryOperator -> String -> Operator Value String
unary op str = Wrap pattern (++)
  where
  pattern :: Pattern Value (String, Value)
  pattern = Pattern $ A.Kleisli match
    where
    match (Unary op' val) | op' == op = Just (str, val)
    match _ = Nothing

binary :: BinaryOperator -> String -> Operator Value String
binary op str = AssocR pattern (\v1 v2 -> v1 ++ " " ++ str ++ " " ++ v2)
  where
  pattern :: Pattern Value (Value, Value)
  pattern = Pattern $ A.Kleisli match
    where
    match (Binary op' v1 v2) | op' == op = Just (v1, v2)
    match _ = Nothing

prettyPrintValue :: Value -> String
prettyPrintValue = fromMaybe (error "Incomplete pattern") . pattern matchValue
  where
  matchValue :: Pattern Value String
  matchValue = buildPrettyPrinter operators (literals <+> fmap parens matchValue)
  operators :: OperatorTable Value String
  operators =
    OperatorTable [ [ Wrap accessor $ \prop val -> val ++ "." ++ prop ]
                  , [ Wrap objectUpdate $ \ps val -> val ++ "{ " ++ intercalate ", " ps ++ " }" ]
                  , [ Wrap app $ \args val -> val ++ "(" ++ args ++ ")" ]
                  , [ Split lam $ \args val -> "\\" ++ intercalate ", " args ++ " -> " ++ prettyPrintValue val ]
                  , [ Wrap ifThenElse $ \(th, el) cond -> cond ++ " ? " ++ prettyPrintValue th ++ " : " ++ prettyPrintValue el ]
                  , [ AssocR indexer (\index val -> val ++ " ! " ++ index) ]
                  , [ binary    LessThan             "<" ]
                  , [ binary    LessThanOrEqualTo    "<=" ]
                  , [ binary    GreaterThan          ">" ]
                  , [ binary    GreaterThanOrEqualTo ">=" ]
                  , [ unary     Not                  "!" ]
                  , [ unary     BitwiseNot           "~" ]
                  , [ unary     Negate               "-" ]
                  , [ binary    Multiply             "*" ]
                  , [ binary    Divide               "/" ]
                  , [ binary    Modulus              "%" ]
                  , [ binary    Concat               "++" ]
                  , [ binary    Add                  "+" ]
                  , [ binary    Subtract             "-" ]
                  , [ binary    ShiftLeft            "<<" ]
                  , [ binary    ShiftRight           ">>" ]
                  , [ binary    ZeroFillShiftRight   ">>>" ]
                  , [ binary    EqualTo              "==" ]
                  , [ binary    NotEqualTo           "!=" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "&&" ]
                  , [ binary    Or                   "||" ]
                  ]

prettyPrintBinder :: Binder -> String
prettyPrintBinder NullBinder = "_"
prettyPrintBinder (StringBinder str) = show str
prettyPrintBinder (NumberBinder num) = either show show num
prettyPrintBinder (BooleanBinder True) = "true"
prettyPrintBinder (BooleanBinder False) = "false"
prettyPrintBinder (VarBinder ident) = show ident
prettyPrintBinder (NullaryBinder ctor) = ctor
prettyPrintBinder (UnaryBinder ctor b) = ctor ++ prettyPrintBinder b
prettyPrintBinder (ObjectBinder bs) = "{ " ++ intercalate ", " (map (uncurry prettyPrintObjectPropertyBinder) bs) ++ " }"
prettyPrintBinder (ArrayBinder bs rest) = "[ " ++ intercalate ", " (map prettyPrintBinder bs) ++ maybe "" (("; " ++) . prettyPrintBinder) rest ++ " ]"
prettyPrintBinder (NamedBinder ident binder) = show ident ++ "@" ++ prettyPrintBinder binder
prettyPrintBinder (GuardedBinder cond binder) = prettyPrintBinder binder ++ " | " ++ prettyPrintValue cond

prettyPrintObjectPropertyBinder :: String -> Binder -> String
prettyPrintObjectPropertyBinder key binder = key ++ ": " ++ prettyPrintBinder binder

prettyPrintObjectProperty :: String -> Value -> String
prettyPrintObjectProperty key value = key ++ ": " ++ prettyPrintValue value

prettyPrintStatement :: Statement -> String
prettyPrintStatement (VariableIntroduction ident value) = "var " ++ show ident ++ " = " ++ prettyPrintValue value
prettyPrintStatement (Assignment target value) = show target ++ " = " ++ prettyPrintValue value
prettyPrintStatement (While cond sts) = "while " ++ prettyPrintValue cond ++ ": {" ++ intercalate ";" (map prettyPrintStatement sts) ++ " }"
prettyPrintStatement (For ident start end sts) = "for " ++ show ident
  ++ " <- " ++ prettyPrintValue start
  ++ " until " ++ prettyPrintValue end ++ ": {"
  ++ intercalate "; " (map prettyPrintStatement sts) ++ " }"
prettyPrintStatement (ForEach ident arr sts) = "foreach " ++ show ident
  ++ " in " ++ prettyPrintValue arr ++ ": {"
  ++ intercalate "; " (map prettyPrintStatement sts) ++ " }"
prettyPrintStatement (If ifst) = prettyPrintIfStatement ifst
prettyPrintStatement (Return value) = "return " ++ prettyPrintValue value

prettyPrintIfStatement :: IfStatement -> String
prettyPrintIfStatement (IfStatement cond thens elst) =
  "if "
  ++ prettyPrintValue cond ++ ": {"
  ++ intercalate "; " (map prettyPrintStatement thens) ++ " }"
  ++ maybe "" prettyPrintElseStatement elst

prettyPrintElseStatement :: ElseStatement -> String
prettyPrintElseStatement (Else sts) = "else: {" ++ intercalate "; " (map prettyPrintStatement sts) ++ " }"
prettyPrintElseStatement (ElseIf ifst) = "else " ++ prettyPrintIfStatement ifst
