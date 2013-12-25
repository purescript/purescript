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

import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import Control.Arrow (second)
import Control.Monad (forM)

import Language.PureScript.TypeChecker (Environment, names)
import Language.PureScript.Values
import Language.PureScript.Names
import Language.PureScript.Declarations
import Language.PureScript.Pretty.Common
import Language.PureScript.CodeGen.Monad
import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.TypeChecker.Monad (NameKind(..))

declToJs :: Maybe Ident -> ModulePath -> Declaration -> Environment -> Maybe [JS]
declToJs curMod mp (ValueDeclaration ident _ _ (Abs args ret)) e =
  Just $ JSFunction (Just ident) args (JSBlock [JSReturn (valueToJs mp e ret)]) :
         maybe [] (return . setProperty (identToJs ident) (JSVar ident)) curMod
declToJs curMod mp (ValueDeclaration ident _ _ val) e =
  Just $ JSVariableIntroduction ident (Just (valueToJs mp e val)) :
         maybe [] (return . setProperty (identToJs ident) (JSVar ident)) curMod
declToJs curMod _ (ExternMemberDeclaration member ident _) _ =
  Just $ JSFunction (Just ident) [Ident "value"] (JSBlock [JSReturn (JSAccessor member (JSVar (Ident "value")))]) :
         maybe [] (return . setProperty (show ident) (JSVar ident)) curMod
declToJs curMod mp (DataDeclaration _ _ ctors) _ =
  Just $ flip concatMap ctors $ \(pn@(ProperName ctor), maybeTy) ->
    let
      ctorJs =
        case maybeTy of
          Nothing -> JSVariableIntroduction (Ident ctor) (Just (JSObjectLiteral [ ("ctor", JSStringLiteral (show (Qualified mp pn))) ]))
          Just _ -> JSFunction (Just (Ident ctor)) [Ident "value"]
                      (JSBlock [JSReturn
                        (JSObjectLiteral [ ("ctor", JSStringLiteral (show (Qualified mp pn)))
                                         , ("value", JSVar (Ident "value")) ])])
    in ctorJs : maybe [] (return . setProperty ctor (JSVar (Ident ctor))) curMod
declToJs curMod mp (ModuleDeclaration pn@(ProperName name) decls) env =
  Just $ [ JSVariableIntroduction (Ident name) Nothing
         , JSApp (JSFunction Nothing [Ident name]
                             (JSBlock (concat $ mapMaybe (\decl -> declToJs (Just (Ident name)) (subModule mp pn) decl env) decls)))
                 [JSAssignment (JSAssignVariable (Ident name))
                               (JSBinary Or (JSVar (Ident name)) (JSObjectLiteral []))]] ++
         maybe [] (return . setProperty name (JSVar (Ident name))) curMod
declToJs _ _ _ _ = Nothing

setProperty :: String -> JS -> Ident -> JS
setProperty prop val curMod = JSAssignment (JSAssignProperty prop (JSAssignVariable curMod)) val

valueToJs :: ModulePath -> Environment -> Value -> JS
valueToJs _ _ (NumericLiteral n) = JSNumericLiteral n
valueToJs _ _ (StringLiteral s) = JSStringLiteral s
valueToJs _ _ (BooleanLiteral b) = JSBooleanLiteral b
valueToJs m e (ArrayLiteral xs) = JSArrayLiteral (map (valueToJs m e) xs)
valueToJs m e (ObjectLiteral ps) = JSObjectLiteral (map (second (valueToJs m e)) ps)
valueToJs m e (ObjectUpdate o ps) = JSApp (JSAccessor "extend" (JSVar (Ident "Object"))) [ valueToJs m e o, JSObjectLiteral (map (second (valueToJs m e)) ps)]
valueToJs _ _ (Constructor name) = qualifiedToJS runProperName name
valueToJs m e (Block sts) = JSApp (JSFunction Nothing [] (JSBlock (map (statementToJs m e) sts))) []
valueToJs m e (Case value binders) = runGen (bindersToJs m e binders (valueToJs m e value))
valueToJs m e (IfThenElse cond th el) = JSConditional (valueToJs m e cond) (valueToJs m e th) (valueToJs m e el)
valueToJs m e (Accessor prop val) = JSAccessor prop (valueToJs m e val)
valueToJs m e (Indexer index val) = JSIndexer (valueToJs m e index) (valueToJs m e val)
valueToJs m e (App val args) = JSApp (valueToJs m e val) (map (valueToJs m e) args)
valueToJs m e (Abs args val) = JSFunction Nothing args (JSBlock [JSReturn (valueToJs m e val)])
valueToJs m e (Unary op val) = JSUnary op (valueToJs m e val)
valueToJs m e (Binary op v1 v2) = JSBinary op (valueToJs m e v1) (valueToJs m e v2)
valueToJs m e (Var ident) = case M.lookup (qualify m ident) (names e) of
  Just (_, Alias aliasModule aliasIdent) -> qualifiedToJS identToJs (Qualified aliasModule aliasIdent)
  _ -> qualifiedToJS identToJs ident
valueToJs m e (TypedValue val _) = valueToJs m e val
valueToJs _ _ _ = error "Invalid argument to valueToJs"

qualifiedToJS :: (a -> String) -> Qualified a -> JS
qualifiedToJS f (Qualified (ModulePath parts) a) =
  delimited (f a : reverse (map show parts))
 where delimited [part]       = JSVar (Ident (part))
       delimited (part:parts') = JSAccessor part (delimited parts')
       delimited _            = error "Invalid argument to delimited"

bindersToJs :: ModulePath -> Environment -> [(Binder, Value)] -> JS -> Gen JS
bindersToJs m e binders val = do
  valName <- fresh
  jss <- forM binders $ \(binder, result) -> binderToJs m e valName [JSReturn (valueToJs m e result)] binder
  return $ JSApp (JSFunction Nothing [Ident valName] (JSBlock (concat jss ++ [JSThrow (JSStringLiteral "Failed pattern match")])))
                 [val]

binderToJs :: ModulePath -> Environment -> String -> [JS] -> Binder -> Gen [JS]
binderToJs _ _ _ done NullBinder = return done
binderToJs _ _ varName done (StringBinder str) =
  return [JSIfElse (JSBinary EqualTo (JSVar (Ident varName)) (JSStringLiteral str)) (JSBlock done) Nothing]
binderToJs _ _ varName done (NumberBinder num) =
  return [JSIfElse (JSBinary EqualTo (JSVar (Ident varName)) (JSNumericLiteral num)) (JSBlock done) Nothing]
binderToJs _ _ varName done (BooleanBinder True) =
  return [JSIfElse (JSVar (Ident varName)) (JSBlock done) Nothing]
binderToJs _ _ varName done (BooleanBinder False) =
  return [JSIfElse (JSUnary Not (JSVar (Ident varName))) (JSBlock done) Nothing]
binderToJs _ _ varName done (VarBinder ident) =
  return (JSVariableIntroduction ident (Just (JSVar (Ident varName))) : done)
binderToJs m _ varName done (NullaryBinder ctor) =
  return [JSIfElse (JSBinary EqualTo (JSAccessor "ctor" (JSVar (Ident varName))) (JSStringLiteral (show (uncurry Qualified $ qualify m ctor)))) (JSBlock done) Nothing]
binderToJs m e varName done (UnaryBinder ctor b) = do
  value <- fresh
  js <- binderToJs m e value done b
  return [JSIfElse (JSBinary EqualTo (JSAccessor "ctor" (JSVar (Ident varName))) (JSStringLiteral (show (uncurry Qualified $ qualify m ctor)))) (JSBlock (JSVariableIntroduction (Ident value) (Just (JSAccessor "value" (JSVar (Ident varName)))) : js)) Nothing]
binderToJs m e varName done (ObjectBinder bs) = go done bs
  where
  go :: [JS] -> [(String, Binder)] -> Gen [JS]
  go done' [] = return done'
  go done' ((prop, binder):bs') = do
    propVar <- fresh
    done'' <- go done' bs'
    js <- binderToJs m e propVar done'' binder
    return (JSVariableIntroduction (Ident propVar) (Just (JSAccessor prop (JSVar (Ident varName)))) : js)
binderToJs m e varName done (ArrayBinder bs) = do
  js <- go done 0 bs
  return [JSIfElse (JSBinary EqualTo (JSAccessor "length" (JSVar (Ident varName))) (JSNumericLiteral (Left (fromIntegral $ length bs)))) (JSBlock js) Nothing]
  where
  go :: [JS] -> Integer -> [Binder] -> Gen [JS]
  go done' _ [] = return done'
  go done' index (binder:bs') = do
    elVar <- fresh
    done'' <- go done' (index + 1) bs'
    js <- binderToJs m e elVar done'' binder
    return (JSVariableIntroduction (Ident elVar) (Just (JSIndexer (JSNumericLiteral (Left index)) (JSVar (Ident varName)))) : js)
binderToJs m e varName done (ConsBinder headBinder tailBinder) = do
  headVar <- fresh
  tailVar <- fresh
  js1 <- binderToJs m e headVar done headBinder
  js2 <- binderToJs m e tailVar js1 tailBinder
  return [JSIfElse (JSBinary GreaterThan (JSAccessor "length" (JSVar (Ident varName))) (JSNumericLiteral (Left 0))) (JSBlock
    ( JSVariableIntroduction (Ident headVar) (Just (JSIndexer (JSNumericLiteral (Left 0)) (JSVar (Ident varName)))) :
      JSVariableIntroduction (Ident tailVar) (Just (JSApp (JSAccessor "slice" (JSVar (Ident varName))) [JSNumericLiteral (Left 1)])) :
      js2
    )) Nothing]
binderToJs m e varName done (NamedBinder ident binder) = do
  js <- binderToJs m e varName done binder
  return (JSVariableIntroduction ident (Just (JSVar (Ident varName))) : js)
binderToJs m e varName done (GuardedBinder cond binder) = binderToJs m e varName done' binder
  where
  done' = [JSIfElse (valueToJs m e cond) (JSBlock done) Nothing]

statementToJs :: ModulePath -> Environment -> Statement -> JS
statementToJs m e (VariableIntroduction ident value) = JSVariableIntroduction ident (Just (valueToJs m e value))
statementToJs m e (Assignment target value) = JSAssignment (JSAssignVariable target) (valueToJs m e value)
statementToJs m e (While cond sts) = JSWhile (valueToJs m e cond) (JSBlock (map (statementToJs m e) sts))
statementToJs m e (For ident start end sts) = JSFor ident (valueToJs m e start) (valueToJs m e end) (JSBlock (map (statementToJs m e) sts))
statementToJs m e (ForEach ident arr sts) = JSApp (JSAccessor "forEach" (valueToJs m e arr)) [JSFunction Nothing [ident] (JSBlock (map (statementToJs m e) sts))]
statementToJs m e (If ifst) = ifToJs ifst
  where
  ifToJs :: IfStatement -> JS
  ifToJs (IfStatement cond thens elses) = JSIfElse (valueToJs m e cond) (JSBlock (map (statementToJs m e) thens)) (fmap elseToJs elses)
  elseToJs :: ElseStatement -> JS
  elseToJs (Else sts) = JSBlock (map (statementToJs m e) sts)
  elseToJs (ElseIf elif) = ifToJs elif
statementToJs m e (ValueStatement val) = valueToJs m e val
statementToJs m e (Return value) = JSReturn (valueToJs m e value)
