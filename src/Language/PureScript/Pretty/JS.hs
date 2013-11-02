-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.JS
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.JS (
    prettyPrintJS
) where

import Language.PureScript.Names
import Language.PureScript.Values
import Language.PureScript.Pretty.Common
import Language.PureScript.CodeGen.JS.AST

import Data.List
import Data.Maybe (fromMaybe)
import qualified Control.Arrow as A
import Control.Arrow ((***), (<+>), first, second)

literals :: Pattern JS String
literals = Pattern $ A.Kleisli match
  where
  match (JSNumericLiteral n) = Just $ either show show n
  match (JSStringLiteral s) = Just $ show s
  match (JSBooleanLiteral True) = Just "true"
  match (JSBooleanLiteral False) = Just "false"
  match (JSArrayLiteral xs) = Just $ "[" ++ intercalate ", " (map prettyPrintJS xs) ++ "]"
  match (JSObjectLiteral ps) = Just $ "{ " ++ intercalate ", " (map (\(key, value) -> key ++ ": " ++ prettyPrintJS value) ps) ++ " }"
  match (JSBlock sts) = Just $ "{ " ++ intercalate "; " (map prettyPrintJS sts) ++ " }"
  match (JSVar ident) = Just (identToJs ident)
  match (JSVariableIntroduction ident value) = Just $ "var " ++ identToJs ident ++ " = " ++ prettyPrintJS value
  match (JSAssignment target value) = Just $ identToJs target ++ " = " ++ prettyPrintJS value
  match (JSWhile cond sts) = Just $ "while ("
    ++ prettyPrintJS cond ++ ") { "
    ++ intercalate "; " (map prettyPrintJS sts) ++ " }"
  match (JSFor ident start end sts) = Just $ "for ("
    ++ identToJs ident ++ " = " ++ prettyPrintJS start ++ "; "
    ++ identToJs ident ++ " < " ++ prettyPrintJS end ++ "; "
    ++ identToJs ident ++ "++) { "
    ++ intercalate "; " (map prettyPrintJS sts) ++ " }"
  match (JSIfElse cond thens elses) = Just $ "if ("
    ++ prettyPrintJS cond ++ ") { "
    ++ intercalate "; " (map prettyPrintJS thens) ++ " }"
    ++ maybe "" ((" else " ++) . prettyPrintJS) elses
  match (JSReturn value) = Just $ "return " ++ prettyPrintJS value
  match (JSThrow value) = Just $ "throw " ++ prettyPrintJS value
  match _ = Nothing

conditional :: Pattern JS ((JS, JS), JS)
conditional = Pattern $ A.Kleisli match
  where
  match (JSConditional cond th el) = Just ((th, el), cond)
  match _ = Nothing

accessor :: Pattern JS (String, JS)
accessor = Pattern $ A.Kleisli match
  where
  match (JSAccessor prop val) = Just (prop, val)
  match _ = Nothing

indexer :: Pattern JS (String, JS)
indexer = Pattern $ A.Kleisli match
  where
  match (JSIndexer index val) = Just (prettyPrintJS index, val)
  match _ = Nothing

lam :: Pattern JS ((Maybe Ident, [Ident]), JS)
lam = Pattern $ A.Kleisli match
  where
  match (JSFunction name args ret) = Just ((name, args), ret)
  match _ = Nothing

app :: Pattern JS (String, JS)
app = Pattern $ A.Kleisli match
  where
  match (JSApp val args) = Just (intercalate "," (map prettyPrintJS args), val)
  match _ = Nothing

unary :: UnaryOperator -> String -> Operator JS String
unary op str = Wrap pattern (++)
  where
  pattern :: Pattern JS (String, JS)
  pattern = Pattern $ A.Kleisli match
    where
    match (JSUnary op' val) | op' == op = Just (str, val)
    match _ = Nothing

binary :: BinaryOperator -> String -> Operator JS String
binary op str = AssocR pattern (\v1 v2 -> v1 ++ " " ++ str ++ " " ++ v2)
  where
  pattern :: Pattern JS (JS, JS)
  pattern = Pattern $ A.Kleisli match
    where
    match (JSBinary op' v1 v2) | op' == op = Just (v1, v2)
    match _ = Nothing

prettyPrintJS :: JS -> String
prettyPrintJS = fromMaybe (error "Incomplete pattern") . pattern matchValue
  where
  matchValue :: Pattern JS String
  matchValue = buildPrettyPrinter operators (literals <+> fmap parens matchValue)
  operators :: OperatorTable JS String
  operators =
    OperatorTable [ [ Wrap accessor $ \prop val -> val ++ "." ++ prop ]
                  , [ Wrap indexer $ \index val -> val ++ "[" ++ index ++ "]" ]
                  , [ Wrap app $ \args val -> val ++ "(" ++ args ++ ")" ]
                  , [ Wrap lam $ \(name, args) ret -> "function "
                        ++ maybe "" identToJs name
                        ++ "(" ++ intercalate "," (map identToJs args) ++ ") "
                        ++ ret ]
                  , [ Wrap conditional $ \(th, el) cond -> cond ++ " ? " ++ prettyPrintJS th ++ " : " ++ prettyPrintJS el ]
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
                  , [ binary    Concat               "+" ]
                  , [ binary    Add                  "+" ]
                  , [ binary    Subtract             "-" ]
                  , [ binary    ShiftLeft            "<<" ]
                  , [ binary    ShiftRight           ">>" ]
                  , [ binary    ZeroFillShiftRight   ">>>" ]
                  , [ binary    EqualTo              "===" ]
                  , [ binary    NotEqualTo           "!==" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "&&" ]
                  , [ binary    Or                   "||" ]
                    ]
