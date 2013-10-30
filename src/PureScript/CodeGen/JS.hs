-----------------------------------------------------------------------------
--
-- Module      :  PureScript.CodeGen.JS
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

module PureScript.CodeGen.JS (
    declToJs
) where

import Data.Char
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Control.Arrow as A
import Control.Arrow ((<+>))
import Control.Applicative

import PureScript.Types
import PureScript.Values
import PureScript.Names
import PureScript.Declarations
import PureScript.CodeGen.Common
import PureScript.CodeGen.Common.Gen

declToJs :: Declaration -> Maybe String
declToJs (ValueDeclaration ident (Abs args ret)) = Just $ "function " ++ identToJs ident ++ "(" ++ intercalate "," (map identToJs args) ++ ") { return " ++ valueToJs ret ++ "; }"
declToJs (ValueDeclaration ident val) = Just $ "var " ++ identToJs ident ++ " = " ++ valueToJs val ++ ";"
declToJs (DataDeclaration _ _ ctors) =
  Just $ flip concatMap ctors $ \(ctor, maybeTy) ->
    case maybeTy of
      Nothing -> "var " ++ ctor ++ " =  { ctor: '" ++ ctor ++ "' };"
      Just _ -> "var " ++ ctor ++ " = function (value) { return { ctor: '" ++ ctor ++ "', value: value }; };"
declToJs _ = Nothing

literals :: Pattern Value String
literals = Pattern $ A.Kleisli match
  where
  match (NumericLiteral n) = Just $ either show show n
  match (StringLiteral s) = Just $ show s
  match (BooleanLiteral True) = Just "true"
  match (BooleanLiteral False) = Just "false"
  match (ArrayLiteral xs) = Just $ "[" ++ intercalate "," (map valueToJs xs) ++ "]"
  match (ObjectLiteral ps) = Just $ "{" ++ intercalate "," (map objectPropertyToJs ps) ++ "}"
  match (Constructor name) = Just name
  match (Block sts) = Just $ "(function () {" ++ intercalate ";" (map statementToJs sts) ++ "})()"
  match (Case value binders) = Just $ "(" ++ runGen (bindersToJs binders) ++  ")(" ++ valueToJs value ++ ")"
    where
    bindersToJs ::  [(Binder, Value)] -> Gen String
    bindersToJs binders = do
      valName <- fresh
      jss <- flip mapM binders $ \(binder, result) -> do
         let js = valueToJs result
         binderToJs valName ("return " ++ js ++ ";") binder
      return $ "function (" ++ valName ++ ") {" ++ concat jss ++ "throw \"Failed pattern match\"; }"
  match (Var ident) = Just (identToJs ident)
  match _ = Nothing

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

indexer :: Pattern Value (String, Value)
indexer = Pattern $ A.Kleisli match
  where
  match (Indexer index val) = Just (valueToJs index, val)
  match _ = Nothing

app :: Pattern Value (String, Value)
app = Pattern $ A.Kleisli match
  where
  match (App val args) = Just (intercalate "," (map valueToJs args), val)
  match _ = Nothing

lam :: Pattern Value ([String], Value)
lam = Pattern $ A.Kleisli match
  where
  match (Abs args val) = Just (map identToJs args, val)
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

valueToJs :: Value -> String
valueToJs = fromMaybe (error "Incomplete pattern") . pattern matchValue
  where
  matchValue :: Pattern Value String
  matchValue = buildPrettyPrinter operators (literals <+> fmap parens matchValue)
  operators :: OperatorTable Value String
  operators =
    OperatorTable $ [ [ Wrap accessor $ \prop val -> val ++ "." ++ prop ]
                    , [ Wrap indexer $ \index val -> val ++ "[" ++ index ++ "]" ]
                    , [ Wrap app $ \args val -> val ++ "(" ++ args ++ ")" ]
                    , [ Split lam $ \args val -> "function (" ++ intercalate "," args ++ ") { return " ++ valueToJs val ++ "; }" ]
                    , [ Wrap ifThenElse $ \(th, el) cond -> cond ++ " ? " ++ valueToJs th ++ " : " ++ valueToJs el ]
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

binderToJs :: String -> String -> Binder -> Gen String
binderToJs varName done NullBinder = return done
binderToJs varName done (StringBinder str) =
  return $ "if (" ++ varName ++ " === \"" ++ str ++ "\") {" ++ done ++ " }"
binderToJs varName done (NumberBinder num) =
  return $ "if (" ++ varName ++ " === " ++ either show show num ++ ") {" ++ done ++ " }"
binderToJs varName done (BooleanBinder True) =
  return $ "if (" ++ varName ++ ") {" ++ done ++ " }"
binderToJs varName done (BooleanBinder False) =
  return $ "if (!" ++ varName ++ ") {" ++ done ++ " }"
binderToJs varName done (VarBinder ident) =
  return $ "var " ++ identToJs ident ++ " = " ++ varName ++ "; " ++ done
binderToJs varName done (NullaryBinder ctor) =
  return $ "if (" ++ varName ++ ".ctor === \"" ++ ctor ++ "\") { " ++ done ++ " }"
binderToJs varName done (UnaryBinder ctor b) = do
  value <- fresh
  js <- binderToJs value done b
  return $ "if (" ++ varName ++ ".ctor === \"" ++ ctor ++ "\") { " ++ "var " ++ value ++ " = " ++ varName ++ ".value; " ++ js ++ " }"
binderToJs varName done (ObjectBinder bs) = go done bs
  where
  go done [] = return done
  go done ((prop, binder):bs) = do
    propVar <- fresh
    done' <- go done bs
    js <- binderToJs propVar done' binder
    return $ "var " ++ propVar ++ " = " ++ varName ++ "." ++ prop ++ ";" ++ js
binderToJs varName done (ArrayBinder bs rest) = do
  js <- go done rest 0 bs
  return $ "if (" ++ varName ++ ".length " ++ cmp ++ " " ++ show (length bs) ++ ") { " ++ js ++ " }"
  where
  cmp = maybe "===" (const ">=") rest
  go done Nothing _ [] = return done
  go done (Just binder) index [] = do
    restVar <- fresh
    js <- binderToJs restVar done binder
    return $ "var " ++ restVar ++ " = " ++ varName ++ ".slice(" ++ show index ++ "); " ++ js
  go done rest index (binder:bs) = do
    elVar <- fresh
    done' <- go done rest (index + 1) bs
    js <- binderToJs elVar done' binder
    return $ "var " ++ elVar ++ " = " ++ varName ++ "[" ++ show index ++ "]; " ++ js
binderToJs varName done (NamedBinder ident binder) = do
  js <- binderToJs varName done binder
  return $ "var " ++ identToJs ident ++ " = " ++ varName ++ "; " ++ js
binderToJs varName done (GuardedBinder cond binder) = binderToJs varName done' binder
  where
  done' = "if (" ++ valueToJs cond ++ ") { " ++ done ++ "}"

objectPropertyToJs :: (String, Value) -> String
objectPropertyToJs (key, value) = key ++ ":" ++ valueToJs value

statementToJs :: Statement -> String
statementToJs (VariableIntroduction ident value) = "var " ++ identToJs ident ++ " = " ++ valueToJs value
statementToJs (Assignment target value) = identToJs target ++ " = " ++ valueToJs value
statementToJs (While cond sts) = "while ("
  ++ valueToJs cond ++ ") {"
  ++ intercalate ";" (map statementToJs sts) ++ "}"
statementToJs (For ident start end sts) = "for (" ++
  identToJs ident ++ " = " ++ valueToJs start ++ ";"
  ++ identToJs ident ++ " < " ++ valueToJs end ++ ";"
  ++ identToJs ident ++ "++) {"
  ++ intercalate ";" (map statementToJs sts) ++ "}"
statementToJs (ForEach ident arr sts) = valueToJs arr
  ++ ".forEach(function(" ++ identToJs ident ++ ") {"
  ++ intercalate ";" (map statementToJs sts) ++ "})"
statementToJs (If ifst) = ifStatementToJs ifst
statementToJs (Return value) = "return " ++ valueToJs value

ifStatementToJs :: IfStatement -> String
ifStatementToJs (IfStatement cond thens elst) =
  "if ("
  ++ valueToJs cond ++ ") {"
  ++ intercalate ";" (map statementToJs thens) ++ "}"
  ++ maybe "" elseStatementToJs elst

elseStatementToJs :: ElseStatement -> String
elseStatementToJs (Else sts) = " else {" ++ intercalate ";" (map statementToJs sts) ++ "}"
elseStatementToJs (ElseIf ifst) = " else " ++ ifStatementToJs ifst
