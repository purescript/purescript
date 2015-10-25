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

{-# LANGUAGE CPP #-}

module Language.PureScript.Pretty.JS (
    prettyPrintJS
) where

import Data.List
import Data.Maybe (fromMaybe)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Arrow ((<+>))
import Control.Monad.State
import Control.PatternArrows
import qualified Control.Arrow as A

import Language.PureScript.Crash
import Language.PureScript.CodeGen.JS.AST
import Language.PureScript.CodeGen.JS.Common
import Language.PureScript.Pretty.Common
import Language.PureScript.Comments

import Numeric

literals :: Pattern PrinterState JS String
literals = mkPattern' match
  where
  match :: JS -> StateT PrinterState Maybe String
  match (JSNumericLiteral n) = return $ either show show n
  match (JSStringLiteral s) = return $ string s
  match (JSBooleanLiteral True) = return "true"
  match (JSBooleanLiteral False) = return "false"
  match (JSArrayLiteral xs) = concat <$> sequence
    [ return "[ "
    , intercalate ", " <$> forM xs prettyPrintJS'
    , return " ]"
    ]
  match (JSObjectLiteral []) = return "{}"
  match (JSObjectLiteral ps) = concat <$> sequence
    [ return "{\n"
    , withIndent $ do
        jss <- forM ps $ \(key, value) -> fmap ((objectPropertyToString key ++ ": ") ++) . prettyPrintJS' $ value
        indentString <- currentIndent
        return $ intercalate ", \n" $ map (indentString ++) jss
    , return "\n"
    , currentIndent
    , return "}"
    ]
    where
    objectPropertyToString :: String -> String
    objectPropertyToString s | identNeedsEscaping s = show s
                             | otherwise = s
  match (JSBlock sts) = concat <$> sequence
    [ return "{\n"
    , withIndent $ prettyStatements sts
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (JSVar ident) = return ident
  match (JSVariableIntroduction ident value) = concat <$> sequence
    [ return "var "
    , return ident
    , maybe (return "") (fmap (" = " ++) . prettyPrintJS') value
    ]
  match (JSAssignment target value) = concat <$> sequence
    [ prettyPrintJS' target
    , return " = "
    , prettyPrintJS' value
    ]
  match (JSWhile cond sts) = concat <$> sequence
    [ return "while ("
    , prettyPrintJS' cond
    , return ") "
    , prettyPrintJS' sts
    ]
  match (JSFor ident start end sts) = concat <$> sequence
    [ return $ "for (var " ++ ident ++ " = "
    , prettyPrintJS' start
    , return $ "; " ++ ident ++ " < "
    , prettyPrintJS' end
    , return $ "; " ++ ident ++ "++) "
    , prettyPrintJS' sts
    ]
  match (JSForIn ident obj sts) = concat <$> sequence
    [ return $ "for (var " ++ ident ++ " in "
    , prettyPrintJS' obj
    , return ") "
    , prettyPrintJS' sts
    ]
  match (JSIfElse cond thens elses) = concat <$> sequence
    [ return "if ("
    , prettyPrintJS' cond
    , return ") "
    , prettyPrintJS' thens
    , maybe (return "") (fmap (" else " ++) . prettyPrintJS') elses
    ]
  match (JSReturn value) = concat <$> sequence
    [ return "return "
    , prettyPrintJS' value
    ]
  match (JSThrow value) = concat <$> sequence
    [ return "throw "
    , prettyPrintJS' value
    ]
  match (JSBreak lbl) = return $ "break " ++ lbl
  match (JSContinue lbl) = return $ "continue " ++ lbl
  match (JSLabel lbl js) = concat <$> sequence
    [ return $ lbl ++ ": "
    , prettyPrintJS' js
    ]
  match (JSComment com js) = fmap concat $ sequence $
    [ return "\n"
    , currentIndent
    , return "/**\n"
    ] ++
    map asLine (concatMap commentLines com) ++
    [ currentIndent
    , return " */\n"
    , currentIndent
    , prettyPrintJS' js
    ]
    where
    commentLines :: Comment -> [String]
    commentLines (LineComment s) = [s]
    commentLines (BlockComment s) = lines s

    asLine :: String -> StateT PrinterState Maybe String
    asLine s = do
      i <- currentIndent
      return $ i ++ " * " ++ removeComments s ++ "\n"

    removeComments :: String -> String
    removeComments ('*' : '/' : s) = removeComments s
    removeComments (c : s) = c : removeComments s

    removeComments [] = []
  match (JSRaw js) = return js
  match _ = mzero

string :: String -> String
string s = '"' : concatMap encodeChar s ++ "\""
  where
  encodeChar :: Char -> String
  encodeChar '\b' = "\\b"
  encodeChar '\t' = "\\t"
  encodeChar '\n' = "\\n"
  encodeChar '\v' = "\\v"
  encodeChar '\f' = "\\f"
  encodeChar '\r' = "\\r"
  encodeChar '"'  = "\\\""
  encodeChar '\\' = "\\\\"
  encodeChar c | fromEnum c > 0xFFF = "\\u" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c > 0xFF = "\\u0" ++ showHex (fromEnum c) ""
  encodeChar c | fromEnum c > 0x7E || fromEnum c < 0x20 = "\\x" ++ showHex (fromEnum c) ""
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

instanceOf :: Pattern PrinterState JS (JS, JS)
instanceOf = mkPattern match
  where
  match (JSInstanceOf val ty) = Just (val, ty)
  match _ = Nothing

unary' :: UnaryOperator -> (JS -> String) -> Operator PrinterState JS String
unary' op mkStr = Wrap match (++)
  where
  match :: Pattern PrinterState JS (String, JS)
  match = mkPattern match'
    where
    match' (JSUnary op' val) | op' == op = Just (mkStr val, val)
    match' _ = Nothing

unary :: UnaryOperator -> String -> Operator PrinterState JS String
unary op str = unary' op (const str)

negateOperator :: Operator PrinterState JS String
negateOperator = unary' Negate (\v -> if isNegate v then "- " else "-")
  where
  isNegate (JSUnary Negate _) = True
  isNegate _ = False

binary :: BinaryOperator -> String -> Operator PrinterState JS String
binary op str = AssocL match (\v1 v2 -> v1 ++ " " ++ str ++ " " ++ v2)
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
prettyPrintJS1 = fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyPrintJS'

-- |
-- Generate a pretty-printed string representing a collection of Javascript expressions at the same indentation level
--
prettyPrintJS :: [JS] -> String
prettyPrintJS = fromMaybe (internalError "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyStatements

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
                  , [ unary JSNew "new " ]
                  , [ Wrap lam $ \(name, args) ret -> "function "
                        ++ fromMaybe "" name
                        ++ "(" ++ intercalate ", " args ++ ") "
                        ++ ret ]
                  , [ Wrap typeOf $ \_ s -> "typeof " ++ s ]
                  , [ unary     Not                  "!"
                    , unary     BitwiseNot           "~"
                    , unary     Positive             "+"
                    , negateOperator ]
                  , [ binary    Multiply             "*"
                    , binary    Divide               "/"
                    , binary    Modulus              "%" ]
                  , [ binary    Add                  "+"
                    , binary    Subtract             "-" ]
                  , [ binary    ShiftLeft            "<<"
                    , binary    ShiftRight           ">>"
                    , binary    ZeroFillShiftRight   ">>>" ]
                  , [ binary    LessThan             "<"
                    , binary    LessThanOrEqualTo    "<="
                    , binary    GreaterThan          ">"
                    , binary    GreaterThanOrEqualTo ">="
                    , AssocR instanceOf $ \v1 v2 -> v1 ++ " instanceof " ++ v2 ]
                  , [ binary    EqualTo              "==="
                    , binary    NotEqualTo           "!==" ]
                  , [ binary    BitwiseAnd           "&" ]
                  , [ binary    BitwiseXor           "^" ]
                  , [ binary    BitwiseOr            "|" ]
                  , [ binary    And                  "&&" ]
                  , [ binary    Or                   "||" ]
                  , [ Wrap conditional $ \(th, el) cond -> cond ++ " ? " ++ prettyPrintJS1 th ++ " : " ++ prettyPrintJS1 el ]
                    ]
