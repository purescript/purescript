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
-- Pretty printer for the Javascript AST
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.JS (
    prettyPrintJS
) where

import Language.PureScript.Pretty.Common
import Language.PureScript.CodeGen.JS.AST

import Data.List
import Data.Maybe (fromMaybe)
import qualified Control.Arrow as A
import Control.Arrow ((<+>))
import Control.PatternArrows
import Control.Applicative
import Control.Monad.State
import Numeric

newtype PrinterState = PrinterState { indent :: Int } deriving (Show, Eq, Ord)

-- |
-- Number of characters per identation level
--
blockIndent :: Int
blockIndent = 4

-- |
-- Pretty print with a new indentation level
--
withIndent :: StateT PrinterState Maybe String -> StateT PrinterState Maybe String
withIndent action = do
  modify $ \st -> st { indent = indent st + blockIndent }
  result <- action
  modify $ \st -> st { indent = indent st - blockIndent }
  return result

-- |
-- Get the current indentation level
--
currentIndent :: StateT PrinterState Maybe String
currentIndent = do
  current <- get
  return $ replicate (indent current) ' '

literals :: Pattern PrinterState JS String
literals = mkPattern' match
  where
  match :: JS -> StateT PrinterState Maybe String
  match (JSNumericLiteral n) = return $ either show show n
  match (JSStringLiteral s) = return $ '"' : concatMap encodeChar s ++ "\""
  match (JSBooleanLiteral True) = return "true"
  match (JSBooleanLiteral False) = return "false"
  match (JSArrayLiteral xs) = fmap concat $ sequence
    [ return "[ "
    , fmap (intercalate ", ") $ forM xs prettyPrintJS'
    , return " ]"
    ]
  match (JSObjectLiteral []) = return "{}"
  match (JSObjectLiteral ps) = fmap concat $ sequence
    [ return "{\n"
    , withIndent $ do
        jss <- forM ps $ \(key, value) -> fmap ((key ++ ": ") ++) . prettyPrintJS' $ value
        indentString <- currentIndent
        return $ intercalate ", \n" $ map (indentString ++) jss
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (JSBlock sts) = fmap concat $ sequence
    [ return "{\n"
    , withIndent $ prettyStatements sts
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (JSVar ident) = return ident
  match (JSVariableIntroduction ident value) = fmap concat $ sequence
    [ return "var "
    , return ident
    , maybe (return "") (fmap (" = " ++) . prettyPrintJS') value
    ]
  match (JSAssignment target value) = fmap concat $ sequence
    [ prettyPrintJS' target
    , return " = "
    , prettyPrintJS' value
    ]
  match (JSWhile cond sts) = fmap concat $ sequence
    [ return "while ("
    , prettyPrintJS' cond
    , return ") "
    , prettyPrintJS' sts
    ]
  match (JSFor ident start end sts) = fmap concat $ sequence
    [ return $ "for (var " ++ ident ++ " = "
    , prettyPrintJS' start
    , return $ "; " ++ ident ++ " < "
    , prettyPrintJS' end
    , return $ "; " ++ ident ++ "++) "
    , prettyPrintJS' sts
    ]
  match (JSForIn ident obj sts) = fmap concat $ sequence
    [ return $ "for (var " ++ ident ++ " in "
    , prettyPrintJS' obj
    , return ") "
    , prettyPrintJS' sts
    ]
  match (JSIfElse cond thens elses) = fmap concat $ sequence
    [ return "if ("
    , prettyPrintJS' cond
    , return ") "
    , prettyPrintJS' thens
    , maybe (return "") (fmap (" else " ++) . prettyPrintJS') elses
    ]
  match (JSReturn value) = fmap concat $ sequence
    [ return "return "
    , prettyPrintJS' value
    ]
  match (JSThrow value) = fmap concat $ sequence
    [ return "throw "
    , prettyPrintJS' value
    ]
  match (JSBreak lbl) = return $ "break " ++ lbl
  match (JSContinue lbl) = return $ "continue " ++ lbl
  match (JSLabel lbl js) = fmap concat $ sequence
    [ return $ lbl ++ ": "
    , prettyPrintJS' js
    ]
  match (JSRaw js) = return js
  match _ = mzero

  encodeChar :: Char -> String
  encodeChar '\r' = "\\r"
  encodeChar '\n' = "\\n"
  encodeChar c | fromEnum c > 0xFFF = "\\u" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c > 0xFF = "\\u0" ++ showHex (fromEnum c) ""
  encodeChar c = [c]

conditional :: Pattern PrinterState JS ((JS, JS), JS)
conditional = mkPattern match
  where
  match (JSConditional cond th el) = Just ((th, el), cond)
  match _ = Nothing

accessor :: Pattern PrinterState JS (String, JS)
accessor = mkPattern match
  where
  match (JSAccessor prop val) = Just (prop, val)
  match _ = Nothing

indexer :: Pattern PrinterState JS (String, JS)
indexer = mkPattern' match
  where
  match (JSIndexer index val) = (,) <$> prettyPrintJS' index <*> pure val
  match _ = mzero

lam :: Pattern PrinterState JS ((Maybe String, [String]), JS)
lam = mkPattern match
  where
  match (JSFunction name args ret) = Just ((name, args), ret)
  match _ = Nothing

app :: Pattern PrinterState JS (String, JS)
app = mkPattern' match
  where
  match (JSApp val args) = do
    jss <- mapM prettyPrintJS' args
    return (intercalate ", " jss, val)
  match _ = mzero

typeOf :: Pattern PrinterState JS ((), JS)
typeOf = mkPattern match
  where
  match (JSTypeOf val) = Just ((), val)
  match _ = Nothing

unary :: UnaryOperator -> String -> Operator PrinterState JS String
unary op str = Wrap match (++)
  where
  match :: Pattern PrinterState JS (String, JS)
  match = mkPattern match'
    where
    match' (JSUnary op' val) | op' == op = Just (str, val)
    match' _ = Nothing

binary :: BinaryOperator -> String -> Operator PrinterState JS String
binary op str = AssocR match (\v1 v2 -> v1 ++ " " ++ str ++ " " ++ v2)
  where
  match :: Pattern PrinterState JS (JS, JS)
  match = mkPattern match'
    where
    match' (JSBinary op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

prettyStatements :: [JS] -> StateT PrinterState Maybe String
prettyStatements sts = do
  jss <- forM sts prettyPrintJS'
  indentString <- currentIndent
  return $ intercalate "\n" $ map ((++ ";") . (indentString ++)) jss

-- |
-- Generate a pretty-printed string representing a Javascript expression
--
prettyPrintJS1 :: JS -> String
prettyPrintJS1 = fromMaybe (error "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyPrintJS'

-- |
-- Generate a pretty-printed string representing a collection of Javascript expressions at the same indentation level
--
prettyPrintJS :: [JS] -> String
prettyPrintJS = fromMaybe (error "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements

-- |
-- Generate an indented, pretty-printed string representing a Javascript expression
--
prettyPrintJS' :: JS -> StateT PrinterState Maybe String
prettyPrintJS' = A.runKleisli $ runPattern matchValue
  where
  matchValue :: Pattern PrinterState JS String
  matchValue = buildPrettyPrinter operators (literals <+> fmap parens matchValue)
  operators :: OperatorTable PrinterState JS String
  operators =
    OperatorTable [ [ Wrap accessor $ \prop val -> val ++ "." ++ prop ]
                  , [ Wrap indexer $ \index val -> val ++ "[" ++ index ++ "]" ]
                  , [ Wrap app $ \args val -> val ++ "(" ++ args ++ ")" ]
                  , [ Wrap lam $ \(name, args) ret -> "function "
                        ++ fromMaybe "" name
                        ++ "(" ++ intercalate ", " args ++ ") "
                        ++ ret ]
                  , [ Wrap conditional $ \(th, el) cond -> cond ++ " ? " ++ prettyPrintJS1 th ++ " : " ++ prettyPrintJS1 el ]
                  , [ binary    LessThan             "<" ]
                  , [ binary    LessThanOrEqualTo    "<=" ]
                  , [ binary    GreaterThan          ">" ]
                  , [ binary    GreaterThanOrEqualTo ">=" ]
                  , [ Wrap typeOf $ \_ s -> "typeof " ++ s ]
                  , [ unary     Not                  "!" ]
                  , [ unary     BitwiseNot           "~" ]
                  , [ unary     Negate               "-" ]
                  , [ unary     Positive             "+" ]
                  , [ binary    Multiply             "*" ]
                  , [ binary    Divide               "/" ]
                  , [ binary    Modulus              "%" ]
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
