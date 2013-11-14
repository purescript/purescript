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
import Data.Maybe (fromMaybe, mapMaybe)
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

declToJs :: Maybe Ident -> ModulePath -> Declaration -> Maybe [JS]
declToJs mod mp (ValueDeclaration ident (Abs args ret)) =
  Just $ JSFunction (Just ident) args (JSBlock [JSReturn (valueToJs mp ret)]) :
         maybe [] (return . setProperty (identToJs ident) (JSVar ident)) mod
declToJs mod mp (ValueDeclaration ident val) =
  Just $ JSVariableIntroduction ident (Just (valueToJs mp val)) :
         maybe [] (return . setProperty (identToJs ident) (JSVar ident)) mod
declToJs mod _ (ExternMemberDeclaration member ident _) =
  Just $ JSFunction (Just ident) [Ident "value"] (JSBlock [JSReturn (JSAccessor member (JSVar (Ident "value")))]) :
         maybe [] (return . setProperty (show ident) (JSVar ident)) mod
declToJs mod mp (DataDeclaration _ _ ctors) =
  Just $ flip concatMap ctors $ \(pn@(ProperName ctor), maybeTy) ->
    let
      ctorJs =
        case maybeTy of
          Nothing -> JSVariableIntroduction (Ident ctor) (Just (JSObjectLiteral [ ("ctor", JSStringLiteral (show (Qualified mp pn))) ]))
          Just _ -> JSFunction (Just (Ident ctor)) [Ident "value"]
                      (JSBlock [JSReturn
                        (JSObjectLiteral [ ("ctor", JSStringLiteral (show (Qualified mp pn)))
                                         , ("value", JSVar (Ident "value")) ])])
    in ctorJs : maybe [] (return . setProperty ctor (JSVar (Ident ctor))) mod
declToJs mod mp (ModuleDeclaration pn@(ProperName name) decls) =
  Just $ [ JSVariableIntroduction (Ident name) Nothing
         , JSApp (JSFunction Nothing [Ident name]
                             (JSBlock (concat $ mapMaybe (declToJs (Just (Ident name)) (subModule mp pn)) decls)))
                 [JSAssignment (JSAssignVariable (Ident name) )
                               (JSBinary Or (JSVar (Ident name)) (JSObjectLiteral []))]] ++
         maybe [] (return . setProperty name (JSVar (Ident name))) mod
declToJs _ _ _ = Nothing

setProperty :: String -> JS -> Ident -> JS
setProperty prop val mod = JSAssignment (JSAssignProperty prop (JSAssignVariable mod)) val

valueToJs :: ModulePath -> Value -> JS
valueToJs _ (NumericLiteral n) = JSNumericLiteral n
valueToJs _ (StringLiteral s) = JSStringLiteral s
valueToJs _ (BooleanLiteral b) = JSBooleanLiteral b
valueToJs m (ArrayLiteral xs) = JSArrayLiteral (map (valueToJs m) xs)
valueToJs m (ObjectLiteral ps) = JSObjectLiteral (map (second (valueToJs m)) ps)
valueToJs m (ObjectUpdate o ps) = JSApp (JSAccessor "extend" (JSVar (Ident "Object"))) [ valueToJs m o, JSObjectLiteral (map (second (valueToJs m)) ps)]
valueToJs m (Constructor name) = qualifiedToJS runProperName name
valueToJs m (Block sts) = JSApp (JSFunction Nothing [] (JSBlock (map (statementToJs m) sts))) []
valueToJs m (Case value binders) = runGen (bindersToJs m binders (valueToJs m value))
valueToJs m (IfThenElse cond th el) = JSConditional (valueToJs m cond) (valueToJs m th) (valueToJs m el)
valueToJs m (Accessor prop val) = JSAccessor prop (valueToJs m val)
valueToJs m (Indexer index val) = JSIndexer (valueToJs m index) (valueToJs m val)
valueToJs m (App val args) = JSApp (valueToJs m val) (map (valueToJs m) args)
valueToJs m (Abs args val) = JSFunction Nothing args (JSBlock [JSReturn (valueToJs m val)])
valueToJs m (Unary op val) = JSUnary op (valueToJs m val)
valueToJs m (Binary op v1 v2) = JSBinary op (valueToJs m v1) (valueToJs m v2)
valueToJs m (Var ident) = qualifiedToJS identToJs ident
valueToJs m (TypedValue val _) = valueToJs m val

qualifiedToJS :: (a -> String) -> Qualified a -> JS
qualifiedToJS f (Qualified (ModulePath parts) a) = delimited (f a : reverse (map show parts))
  where
  delimited [part] = JSVar (Ident (part))
  delimited (part:parts) = JSAccessor part (delimited parts)

bindersToJs :: ModulePath -> [(Binder, Value)] -> JS -> Gen JS
bindersToJs m binders val = do
  valName <- fresh
  jss <- forM binders $ \(binder, result) -> binderToJs m valName [JSReturn (valueToJs m result)] binder
  return $ JSApp (JSFunction Nothing [Ident valName] (JSBlock (concat jss ++ [JSThrow (JSStringLiteral "Failed pattern match")])))
                 [val]

binderToJs :: ModulePath -> String -> [JS] -> Binder -> Gen [JS]
binderToJs _ varName done NullBinder = return done
binderToJs _ varName done (StringBinder str) =
  return [JSIfElse (JSBinary EqualTo (JSVar (Ident varName)) (JSStringLiteral str)) (JSBlock done) Nothing]
binderToJs _ varName done (NumberBinder num) =
  return [JSIfElse (JSBinary EqualTo (JSVar (Ident varName)) (JSNumericLiteral num)) (JSBlock done) Nothing]
binderToJs _ varName done (BooleanBinder True) =
  return [JSIfElse (JSVar (Ident varName)) (JSBlock done) Nothing]
binderToJs _ varName done (BooleanBinder False) =
  return [JSIfElse (JSUnary Not (JSVar (Ident varName))) (JSBlock done) Nothing]
binderToJs _ varName done (VarBinder ident) =
  return (JSVariableIntroduction ident (Just (JSVar (Ident varName))) : done)
binderToJs m varName done (NullaryBinder ctor) =
  return [JSIfElse (JSBinary EqualTo (JSAccessor "ctor" (JSVar (Ident varName))) (JSStringLiteral (show (uncurry Qualified $ qualify m ctor)))) (JSBlock done) Nothing]
binderToJs m varName done (UnaryBinder ctor b) = do
  value <- fresh
  js <- binderToJs m value done b
  return [JSIfElse (JSBinary EqualTo (JSAccessor "ctor" (JSVar (Ident varName))) (JSStringLiteral (show (uncurry Qualified $ qualify m ctor)))) (JSBlock (JSVariableIntroduction (Ident value) (Just (JSAccessor "value" (JSVar (Ident varName)))) : js)) Nothing]
binderToJs m varName done (ObjectBinder bs) = go done bs
  where
  go :: [JS] -> [(String, Binder)] -> Gen [JS]
  go done [] = return done
  go done ((prop, binder):bs) = do
    propVar <- fresh
    done' <- go done bs
    js <- binderToJs m propVar done' binder
    return (JSVariableIntroduction (Ident propVar) (Just (JSAccessor prop (JSVar (Ident varName)))) : js)
binderToJs m varName done (ArrayBinder bs rest) = do
  js <- go done rest 0 bs
  return [JSIfElse (JSBinary cmp (JSAccessor "length" (JSVar (Ident varName))) (JSNumericLiteral (Left (fromIntegral $ length bs)))) (JSBlock js) Nothing]
  where
  cmp :: BinaryOperator
  cmp = maybe EqualTo (const GreaterThanOrEqualTo) rest
  go :: [JS] -> Maybe Binder -> Integer -> [Binder] -> Gen [JS]
  go done Nothing _ [] = return done
  go done (Just binder) index [] = do
    restVar <- fresh
    js <- binderToJs m restVar done binder
    return (JSVariableIntroduction (Ident restVar) (Just (JSApp (JSAccessor "slice" (JSVar (Ident varName))) [JSNumericLiteral (Left index)])) : js)
  go done rest index (binder:bs) = do
    elVar <- fresh
    done' <- go done rest (index + 1) bs
    js <- binderToJs m elVar done' binder
    return (JSVariableIntroduction (Ident elVar) (Just (JSIndexer (JSNumericLiteral (Left index)) (JSVar (Ident varName)))) : js)
binderToJs m varName done (NamedBinder ident binder) = do
  js <- binderToJs m varName done binder
  return (JSVariableIntroduction ident (Just (JSVar (Ident varName))) : js)
binderToJs m varName done (GuardedBinder cond binder) = binderToJs m varName done' binder
  where
  done' = [JSIfElse (valueToJs m cond) (JSBlock done) Nothing]

statementToJs :: ModulePath -> Statement -> JS
statementToJs m (VariableIntroduction ident value) = JSVariableIntroduction ident (Just (valueToJs m value))
statementToJs m (Assignment target value) = JSAssignment (JSAssignVariable target) (valueToJs m value)
statementToJs m (While cond sts) = JSWhile (valueToJs m cond) (JSBlock (map (statementToJs m) sts))
statementToJs m (For ident start end sts) = JSFor ident (valueToJs m start) (valueToJs m end) (JSBlock (map (statementToJs m) sts))
statementToJs m (ForEach ident arr sts) = JSApp (JSAccessor "forEach" (valueToJs m arr)) [JSFunction Nothing [ident] (JSBlock (map (statementToJs m) sts))]
statementToJs m (If ifst) = ifToJs ifst
  where
  ifToJs :: IfStatement -> JS
  ifToJs (IfStatement cond thens elses) = JSIfElse (valueToJs m cond) (JSBlock (map (statementToJs m) thens)) (fmap elseToJs elses)
  elseToJs :: ElseStatement -> JS
  elseToJs (Else sts) = JSBlock (map (statementToJs m) sts)
  elseToJs (ElseIf ifst) = ifToJs ifst
statementToJs m (Return value) = JSReturn (valueToJs m value)
