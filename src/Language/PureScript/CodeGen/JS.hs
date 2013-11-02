-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS
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

module Language.PureScript.CodeGen.JS (
    module AST,
    declToJs
) where

import Data.Char
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Control.Arrow as A
import Control.Arrow ((<+>), second)
import Control.Monad (forM)
import Control.Applicative

import Language.PureScript.Types
import Language.PureScript.Values
import Language.PureScript.Names
import Language.PureScript.Declarations
import Language.PureScript.Pretty.Common
import Language.PureScript.CodeGen.Monad
import Language.PureScript.CodeGen.JS.AST as AST

declToJs :: Declaration -> Maybe [JS]
declToJs (ValueDeclaration ident (Abs args ret)) = Just [JSFunction (Just ident) args (JSBlock [JSReturn (valueToJs ret)])]
declToJs (ValueDeclaration ident val) = Just [JSVariableIntroduction ident (valueToJs val)]
declToJs (DataDeclaration _ _ ctors) =
  Just $ flip map ctors $ \(ctor, maybeTy) ->
    case maybeTy of
      Nothing -> JSVariableIntroduction (Ident ctor) (JSObjectLiteral [ ("ctor", JSStringLiteral ctor) ])
      Just _ -> JSFunction (Just (Ident ctor)) [Ident "value"]
                  (JSBlock [JSReturn
                    (JSObjectLiteral [ ("ctor", JSStringLiteral ctor)
                                     , ("value", JSVar (Ident "value")) ])])
declToJs _ = Nothing

valueToJs :: Value -> JS
valueToJs (NumericLiteral n) = JSNumericLiteral n
valueToJs (StringLiteral s) = JSStringLiteral s
valueToJs (BooleanLiteral b) = JSBooleanLiteral b
valueToJs (ArrayLiteral xs) = JSArrayLiteral (map valueToJs xs)
valueToJs (ObjectLiteral ps) = JSObjectLiteral (map (second valueToJs) ps)
valueToJs (ObjectUpdate o ps) = JSApp (JSAccessor "extend" (JSVar (Ident "Object"))) [ valueToJs o, JSObjectLiteral (map (second valueToJs) ps)]
valueToJs (Constructor name) = JSVar (Ident name)
valueToJs (Block sts) = JSApp (JSFunction Nothing [] (JSBlock (map statementToJs sts))) []
valueToJs (Case value binders) = runGen (bindersToJs binders (valueToJs value))
valueToJs (IfThenElse cond th el) = JSConditional (valueToJs cond) (valueToJs th) (valueToJs el)
valueToJs (Accessor prop val) = JSAccessor prop (valueToJs val)
valueToJs (Indexer index val) = JSIndexer (valueToJs index) (valueToJs val)
valueToJs (App val args) = JSApp (valueToJs val) (map valueToJs args)
valueToJs (Abs args val) = JSFunction Nothing args (JSBlock [JSReturn (valueToJs val)])
valueToJs (Unary op val) = JSUnary op (valueToJs val)
valueToJs (Binary op v1 v2) = JSBinary op (valueToJs v1) (valueToJs v2)
valueToJs (Var ident) = JSVar ident
valueToJs (TypedValue val _) = valueToJs val

bindersToJs ::  [(Binder, Value)] -> JS -> Gen JS
bindersToJs binders val = do
  valName <- fresh
  jss <- forM binders $ \(binder, result) -> binderToJs valName [JSReturn (valueToJs result)] binder
  return $ JSApp (JSFunction Nothing [Ident valName] (JSBlock (concat jss ++ [JSThrow (JSStringLiteral "Failed pattern match")])))
                 [val]

binderToJs :: String -> [JS] -> Binder -> Gen [JS]
binderToJs varName done NullBinder = return done
binderToJs varName done (StringBinder str) =
  return [JSIfElse (JSBinary EqualTo (JSVar (Ident varName)) (JSStringLiteral str)) done Nothing]
binderToJs varName done (NumberBinder num) =
  return [JSIfElse (JSBinary EqualTo (JSVar (Ident varName)) (JSNumericLiteral num)) done Nothing]
binderToJs varName done (BooleanBinder True) =
  return [JSIfElse (JSVar (Ident varName)) done Nothing]
binderToJs varName done (BooleanBinder False) =
  return [JSIfElse (JSUnary Not (JSVar (Ident varName))) done Nothing]
binderToJs varName done (VarBinder ident) =
  return (JSVariableIntroduction ident (JSVar (Ident varName)) : done)
binderToJs varName done (NullaryBinder ctor) =
  return [JSIfElse (JSBinary EqualTo (JSAccessor "ctor" (JSVar (Ident varName))) (JSStringLiteral ctor)) done Nothing]
binderToJs varName done (UnaryBinder ctor b) = do
  value <- fresh
  js <- binderToJs value done b
  return [JSIfElse (JSBinary EqualTo (JSAccessor "ctor" (JSVar (Ident varName))) (JSStringLiteral ctor)) (JSVariableIntroduction (Ident value) (JSAccessor "value" (JSVar (Ident varName))) : js) Nothing]
binderToJs varName done (ObjectBinder bs) = go done bs
  where
  go :: [JS] -> [(String, Binder)] -> Gen [JS]
  go done [] = return done
  go done ((prop, binder):bs) = do
    propVar <- fresh
    done' <- go done bs
    js <- binderToJs propVar done' binder
    return (JSVariableIntroduction (Ident propVar) (JSAccessor prop (JSVar (Ident varName))) : js)
binderToJs varName done (ArrayBinder bs rest) = do
  js <- go done rest 0 bs
  return [JSIfElse (JSBinary cmp (JSAccessor "length" (JSVar (Ident varName))) (JSNumericLiteral (Left (fromIntegral $ length bs)))) js Nothing]
  where
  cmp :: BinaryOperator
  cmp = maybe EqualTo (const GreaterThanOrEqualTo) rest
  go :: [JS] -> Maybe Binder -> Integer -> [Binder] -> Gen [JS]
  go done Nothing _ [] = return done
  go done (Just binder) index [] = do
    restVar <- fresh
    js <- binderToJs restVar done binder
    return (JSVariableIntroduction (Ident restVar) (JSApp (JSAccessor "slice" (JSVar (Ident varName))) [JSNumericLiteral (Left index)]) : js)
  go done rest index (binder:bs) = do
    elVar <- fresh
    done' <- go done rest (index + 1) bs
    js <- binderToJs elVar done' binder
    return (JSVariableIntroduction (Ident elVar) (JSIndexer (JSNumericLiteral (Left index)) (JSVar (Ident varName))) : js)
binderToJs varName done (NamedBinder ident binder) = do
  js <- binderToJs varName done binder
  return (JSVariableIntroduction ident (JSVar (Ident varName)) : js)
binderToJs varName done (GuardedBinder cond binder) = binderToJs varName done' binder
  where
  done' = [JSIfElse (valueToJs cond) done Nothing]

statementToJs :: Statement -> JS
statementToJs (VariableIntroduction ident value) = JSVariableIntroduction ident (valueToJs value)
statementToJs (Assignment target value) = JSAssignment target (valueToJs value)
statementToJs (While cond sts) = JSWhile (valueToJs cond) (map statementToJs sts)
statementToJs (For ident start end sts) = JSFor ident (valueToJs start) (valueToJs end) (map statementToJs sts)
statementToJs (ForEach ident arr sts) = JSApp (JSAccessor "forEach" (valueToJs arr)) [JSFunction Nothing [ident] (JSBlock (map statementToJs sts))]
statementToJs (If ifst) = ifToJs ifst
  where
  ifToJs :: IfStatement -> JS
  ifToJs (IfStatement cond thens elses) = JSIfElse (valueToJs cond) (map statementToJs thens) (fmap elseToJs elses)
  elseToJs :: ElseStatement -> JS
  elseToJs (Else sts) = JSBlock (map statementToJs sts)
  elseToJs (ElseIf ifst) = ifToJs ifst
statementToJs (Return value) = JSReturn (valueToJs value)
