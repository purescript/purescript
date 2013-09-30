-----------------------------------------------------------------------------
--
-- Module      :  PureScript.CodeGen
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

module PureScript.CodeGen (
    declToJs
) where

import Data.List (nub, intersperse, intercalate)
import PureScript.Values
import PureScript.Declarations

declToJs :: Declaration -> Maybe String
declToJs (ValueDeclaration name val) = Just $ "var " ++ name ++ " = " ++ valueToJs val ++ ";"
declToJs (DataDeclaration dcs@(DataConstructors { dataConstructors = ctors })) =
  Just $ concatMap (\(ctor, _) -> "var " ++ ctor ++ " = function (value) { return { ctor: '" ++ ctor ++ "', value: value }; };") ctors
declToJs _ = Nothing

valueToJs :: Value -> String
valueToJs (NumericLiteral n) = show n
valueToJs (StringLiteral s) = show s
valueToJs (BooleanLiteral True) = "true"
valueToJs (BooleanLiteral False) = "false"
valueToJs (Unary op value) = unaryOperatorString op ++ "(" ++ valueToJs value ++ ")"
valueToJs (Binary op left right) = "(" ++ valueToJs left ++ ") " ++ binaryOperatorString op ++ " (" ++ valueToJs right ++ ")"
valueToJs (ArrayLiteral xs) = "[" ++ intercalate "," (map valueToJs xs) ++ "]"
valueToJs (ObjectLiteral ps) = "{" ++ intercalate "," (map objectPropertyToJs ps) ++ "}"
valueToJs (Accessor prop val) = "(" ++ valueToJs val ++ ")." ++ prop
valueToJs (Abs args value) = "function (" ++ intercalate "," args ++ ") { return " ++ valueToJs value ++ "; }"
valueToJs (Var ident) = ident
valueToJs (App f xs) = "(" ++ valueToJs f ++ ") (" ++ intercalate "," (map valueToJs xs) ++ ")"
valueToJs (Block sts) = "(function () {" ++ intercalate ";" (map statementToJs sts) ++ "})()"
valueToJs (Constructor name) = name
valueToJs (Case value binders) =
  "(function (_0) {"
  ++ concatMap (\(b, val) -> fst $ binderToJs 0 0 b $ "return " ++ valueToJs val ++ ";") binders
  ++ "throw \"Failed pattern match\";"
  ++ "})(" ++ valueToJs value ++ ")"
valueToJs (TypedValue value _) = valueToJs value

unaryOperatorString :: UnaryOperator -> String
unaryOperatorString Negate = "-"
unaryOperatorString Not = "!"
unaryOperatorString BitwiseNot = "~"

binaryOperatorString :: BinaryOperator -> String
binaryOperatorString Add = "+"
binaryOperatorString Subtract = "-"
binaryOperatorString Multiply = "*"
binaryOperatorString Divide = "/"
binaryOperatorString Modulus = "%"
binaryOperatorString LessThan = "<"
binaryOperatorString LessThanOrEqualTo = "<="
binaryOperatorString GreaterThan = ">"
binaryOperatorString GreaterThanOrEqualTo = ">="
binaryOperatorString BitwiseAnd = "&"
binaryOperatorString BitwiseOr = "|"
binaryOperatorString BitwiseXor = "^"
binaryOperatorString ShiftLeft = "<<"
binaryOperatorString ShiftRight = ">>"
binaryOperatorString ZeroFillShiftRight = ">>>"
binaryOperatorString EqualTo = "==="
binaryOperatorString NotEqualTo = "!=="
binaryOperatorString And = "&&"
binaryOperatorString Or = "||"
binaryOperatorString Concat = "+"

binderToJs :: Int -> Int -> Binder -> String -> (String, Int)
binderToJs varName fresh (VarBinder s) done = ("var " ++ s ++ " = _" ++ show varName ++ "; " ++ done, fresh)
binderToJs varName fresh (ConstructorBinder ctor b) done =
  ("if (_" ++ show varName ++ ".ctor === \"" ++ ctor ++ "\") {"
  ++ "var _" ++ show fresh' ++ " = _" ++ show varName ++ ".value;"
  ++ js
  ++ "}", fresh'')
  where
  fresh' = succ fresh
  (js, fresh'') = binderToJs fresh' fresh' b done
binderToJs varName fresh (ObjectBinder bs) done = go fresh bs done
  where
  go fresh [] done = (done, fresh)
  go fresh ((prop, binder):bs') done =
    ( "var _" ++ show fresh' ++ " = _" ++ show varName ++ "." ++ prop ++ ";"
      ++ js
    , fresh''')
    where
    fresh' = succ fresh
    (done', fresh'') = go fresh' bs' done
    (js, fresh''') = binderToJs fresh' fresh'' binder done'

objectPropertyToJs :: (String, Value) -> String
objectPropertyToJs (key, value) = key ++ ":" ++ valueToJs value

statementToJs :: Statement -> String
statementToJs (VariableIntroduction name value) = "var " ++ name ++ " = " ++ valueToJs value
statementToJs (Assignment target value) = target ++ " = " ++ valueToJs value
statementToJs (While cond sts) = "while ("
  ++ valueToJs cond ++ ") {"
  ++ intercalate ";" (map statementToJs sts) ++ "}"
statementToJs (For (init, cond, done) sts) = "for (" ++
  statementToJs init
  ++ "; " ++ valueToJs cond
  ++ "; " ++ statementToJs done
  ++ ") {" ++ intercalate ";" (map statementToJs sts) ++ "}"
statementToJs (IfThenElse cond thens elses) = "if ("
  ++ valueToJs cond ++ ") {"
  ++ intercalate ";" (map statementToJs thens) ++ "}"
  ++ flip (maybe "") elses (\sts ->
    " else {" ++ intercalate ";" (map statementToJs sts) ++ "}")
statementToJs (Return value) = "return " ++ valueToJs value
