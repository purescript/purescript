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
    declToJs,
    moduleToJs
) where

import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import Control.Arrow (second)
import Control.Monad (replicateM, forM)

import Language.PureScript.TypeChecker (Environment, names)
import Language.PureScript.Values
import Language.PureScript.Names
import Language.PureScript.Scope
import Language.PureScript.Declarations
import Language.PureScript.Pretty.Common
import Language.PureScript.CodeGen.Monad
import Language.PureScript.Options
import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.TypeChecker.Monad (NameKind(..))
import Language.PureScript.Types

moduleToJs :: Options -> Module -> Environment -> [JS]
moduleToJs opts (Module pname@(ProperName name) decls) env =
  [ JSVariableIntroduction (Ident name) Nothing
  , JSApp (JSFunction Nothing [Ident name]
                      (JSBlock (concat $ mapMaybe (\decl -> declToJs opts (ModuleName pname) decl env) decls)))
          [JSAssignment (JSAssignVariable (Ident name))
                         (JSBinary Or (JSVar (Ident name)) (JSObjectLiteral []))]
  ]

declToJs :: Options -> ModuleName -> Declaration -> Environment -> Maybe [JS]
declToJs opts mp (ValueDeclaration ident _ _ val) e =
  Just [ JSVariableIntroduction ident (Just (valueToJs opts mp e val)),
         setProperty (identToJs ident) (JSVar ident) mp ]
declToJs opts mp (BindingGroupDeclaration vals) e =
  Just $ concatMap (\(ident, val) ->
           [ JSVariableIntroduction ident (Just (valueToJs opts mp e val)),
             setProperty (identToJs ident) (JSVar ident) mp ]
         ) vals
declToJs _ mp (ExternMemberDeclaration member ident _) _ =
  Just [ JSFunction (Just ident) [Ident "value"] (JSBlock [JSReturn (JSAccessor member (JSVar (Ident "value")))]),
         setProperty (show ident) (JSVar ident) mp ]
declToJs _ mp (DataDeclaration _ _ ctors) _ =
  Just $ flip concatMap ctors $ \(pn@(ProperName ctor), maybeTy) ->
    let
      ctorJs =
        case maybeTy of
          Nothing -> JSVariableIntroduction (Ident ctor) (Just (JSObjectLiteral [ ("ctor", JSStringLiteral (show (Qualified (Just mp) pn))) ]))
          Just _ -> JSFunction (Just (Ident ctor)) [Ident "value"]
                      (JSBlock [JSReturn
                        (JSObjectLiteral [ ("ctor", JSStringLiteral (show (Qualified (Just mp) pn)))
                                         , ("value", JSVar (Ident "value")) ])])
    in [ ctorJs, setProperty ctor (JSVar (Ident ctor)) mp ]
declToJs _ _ _ _ = Nothing

setProperty :: String -> JS -> ModuleName -> JS
setProperty prop val (ModuleName (ProperName moduleName)) = JSAssignment (JSAssignProperty prop (JSAssignVariable (Ident moduleName))) val

valueToJs :: Options -> ModuleName -> Environment -> Value -> JS
valueToJs _ _ _ (NumericLiteral n) = JSNumericLiteral n
valueToJs _ _ _ (StringLiteral s) = JSStringLiteral s
valueToJs _ _ _ (BooleanLiteral b) = JSBooleanLiteral b
valueToJs opts m e (ArrayLiteral xs) = JSArrayLiteral (map (valueToJs opts m e) xs)
valueToJs opts m e (ObjectLiteral ps) = JSObjectLiteral (map (second (valueToJs opts m e)) ps)
valueToJs opts m e (ObjectUpdate o ps) = JSApp (JSAccessor "extend" (JSVar (Ident "Object"))) [ valueToJs opts m e o, JSObjectLiteral (map (second (valueToJs opts m e)) ps)]
valueToJs _ _ _ (Constructor name) = qualifiedToJS runProperName name
valueToJs opts m e (Block sts) = JSApp (JSFunction Nothing [] (JSBlock (map (statementToJs opts m e) sts))) []
valueToJs opts m e (Case values binders) = runGen (bindersToJs opts m e binders (map (valueToJs opts m e) values))
valueToJs opts m e (IfThenElse cond th el) = JSConditional (valueToJs opts m e cond) (valueToJs opts m e th) (valueToJs opts m e el)
valueToJs opts m e (Accessor prop val) = JSAccessor prop (valueToJs opts m e val)
valueToJs opts m e (Indexer index val) = JSIndexer (valueToJs opts m e index) (valueToJs opts m e val)
valueToJs opts m e (App val args) = JSApp (valueToJs opts m e val) (map (valueToJs opts m e) args)
valueToJs opts m e (Abs args val) = JSFunction Nothing args (JSBlock [JSReturn (valueToJs opts m e val)])
valueToJs opts m e (TypedValue (Abs args val) ty) | optionsPerformRuntimeTypeChecks opts = JSFunction Nothing args (JSBlock $ runtimeTypeChecks args ty ++ [JSReturn (valueToJs opts m e val)])
valueToJs opts m e (Unary op val) = JSUnary op (valueToJs opts m e val)
valueToJs opts m e (Binary op v1 v2) = JSBinary op (valueToJs opts m e v1) (valueToJs opts m e v2)
valueToJs _ m e (Var ident) = varToJs m e ident
valueToJs opts m e (TypedValue val _) = valueToJs opts m e val
valueToJs _ _ _ _ = error "Invalid argument to valueToJs"

runtimeTypeChecks :: [Ident] -> Type -> [JS]
runtimeTypeChecks args ty =
  let
    argTys = getFunctionArgumentTypes ty
  in
    concat $ zipWith argumentCheck (map JSVar args) argTys
  where
  getFunctionArgumentTypes :: Type -> [Type]
  getFunctionArgumentTypes (Function funArgs _) = funArgs
  getFunctionArgumentTypes (ForAll _ ty') = getFunctionArgumentTypes ty'
  getFunctionArgumentTypes _ = []
  argumentCheck :: JS -> Type -> [JS]
  argumentCheck val Number = [typeCheck val "number"]
  argumentCheck val String = [typeCheck val "string"]
  argumentCheck val Boolean = [typeCheck val "boolean"]
  argumentCheck val (Array _) = [arrayCheck val]
  argumentCheck val (Object row) =
    let
      (pairs, _) = rowToList row
    in
      typeCheck val "object" : concatMap (\(prop, ty') -> argumentCheck (JSAccessor prop val) ty') pairs
  argumentCheck val (Function _ _) = [typeCheck val "function"]
  argumentCheck val (ForAll _ ty') = argumentCheck val ty'
  argumentCheck _ _ = []
  typeCheck :: JS -> String -> JS
  typeCheck js ty' = JSIfElse (JSBinary NotEqualTo (JSTypeOf js) (JSStringLiteral ty')) (JSBlock [JSThrow (JSStringLiteral $ ty' ++ " expected")]) Nothing
  arrayCheck :: JS -> JS
  arrayCheck js = JSIfElse (JSUnary Not (JSApp (JSAccessor "isArray" (JSVar (Ident "Array"))) [js])) (JSBlock [JSThrow (JSStringLiteral "Array expected")]) Nothing

varToJs :: ModuleName -> Environment -> Qualified Ident -> JS
varToJs m e qual@(Qualified _ ident) = case M.lookup (qualify m qual) (names e) of
  Just (_, ty) | isExtern ty -> JSVar ident
  Just (_, Alias aliasModule aliasIdent) -> qualifiedToJS identToJs (Qualified (Just aliasModule) aliasIdent)
  _ -> qualifiedToJS identToJs qual
  where
  isExtern Extern = True
  isExtern (Alias m' ident') = case M.lookup (m', ident') (names e) of
    Just (_, ty') -> isExtern ty'
    Nothing -> error "Undefined alias in varToJs"
  isExtern _ = False

qualifiedToJS :: (a -> String) -> Qualified a -> JS
qualifiedToJS f (Qualified (Just (ModuleName (ProperName m))) a) = JSAccessor (f a) (JSVar (Ident m))
qualifiedToJS f (Qualified Nothing a) = JSVar (Ident (f a))

bindersToJs :: Options -> ModuleName -> Environment -> [([Binder], Maybe Guard, Value)] -> [JS] -> Gen JS
bindersToJs opts m e binders vals = do
  setNextName $ firstUnusedName (binders, vals)
  valNames <- replicateM (length vals) fresh
  jss <- forM binders $ \(bs, grd, result) -> go valNames [JSReturn (valueToJs opts m e result)] bs grd
  return $ JSApp (JSFunction Nothing (map Ident valNames) (JSBlock (concat jss ++ [JSThrow (JSStringLiteral "Failed pattern match")])))
                 vals
  where
    go :: [String] -> [JS] -> [Binder] -> Maybe Guard -> Gen [JS]
    go _ done [] Nothing = return done
    go _ done [] (Just cond) = return [JSIfElse (valueToJs opts m e cond) (JSBlock done) Nothing]
    go (v:vs) done' (b:bs) grd = do
      done'' <- go vs done' bs grd
      binderToJs m e v done'' b
    go _ _ _ _ = error "Invalid arguments to bindersToJs"

binderToJs :: ModuleName -> Environment -> String -> [JS] -> Binder -> Gen [JS]
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
  return [JSIfElse (JSBinary EqualTo (JSAccessor "ctor" (JSVar (Ident varName))) (JSStringLiteral (show ((\(mp, nm) -> Qualified (Just mp) nm) $ qualify m ctor)))) (JSBlock done) Nothing]
binderToJs m e varName done (UnaryBinder ctor b) = do
  value <- fresh
  js <- binderToJs m e value done b
  return [JSIfElse (JSBinary EqualTo (JSAccessor "ctor" (JSVar (Ident varName))) (JSStringLiteral (show ((\(mp, nm) -> Qualified (Just mp) nm) $ qualify m ctor)))) (JSBlock (JSVariableIntroduction (Ident value) (Just (JSAccessor "value" (JSVar (Ident varName)))) : js)) Nothing]
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

statementToJs :: Options -> ModuleName -> Environment -> Statement -> JS
statementToJs opts m e (VariableIntroduction ident value) = JSVariableIntroduction ident (Just (valueToJs opts m e value))
statementToJs opts m e (Assignment target value) = JSAssignment (JSAssignVariable target) (valueToJs opts m e value)
statementToJs opts m e (While cond sts) = JSWhile (valueToJs opts m e cond) (JSBlock (map (statementToJs opts m e) sts))
statementToJs opts m e (For ident start end sts) = JSFor ident (valueToJs opts m e start) (valueToJs opts m e end) (JSBlock (map (statementToJs opts m e) sts))
statementToJs opts m e (If ifst) = ifToJs ifst
  where
  ifToJs :: IfStatement -> JS
  ifToJs (IfStatement cond thens elses) = JSIfElse (valueToJs opts m e cond) (JSBlock (map (statementToJs opts m e) thens)) (fmap elseToJs elses)
  elseToJs :: ElseStatement -> JS
  elseToJs (Else sts) = JSBlock (map (statementToJs opts m e) sts)
  elseToJs (ElseIf elif) = ifToJs elif
statementToJs opts m e (Return value) = JSReturn (valueToJs opts m e value)
