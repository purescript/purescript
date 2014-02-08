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
-- This module generates code in the simplified Javascript intermediate representation from Purescript code
--
-----------------------------------------------------------------------------

{-# LANGUAGE DoAndIfThenElse #-}

module Language.PureScript.CodeGen.JS (
    module AST,
    declToJs,
    moduleToJs
) where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (sortBy)
import Data.Function (on)

import Control.Arrow (second)
import Control.Monad (replicateM, forM)

import qualified Data.Map as M

import Language.PureScript.TypeChecker (Environment(..), NameKind(..))
import Language.PureScript.Values
import Language.PureScript.Names
import Language.PureScript.Scope
import Language.PureScript.Declarations
import Language.PureScript.Pretty.Common
import Language.PureScript.CodeGen.Monad
import Language.PureScript.Options
import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.Types
import Language.PureScript.CodeGen.Optimize
import Language.PureScript.CodeGen.Common
import Language.PureScript.TypeChecker.Monad (canonicalizeDataConstructor)

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a module
--
moduleToJs :: Options -> Module -> Environment -> [JS]
moduleToJs opts (Module pname@(ProperName name) decls) env =
  [ JSVariableIntroduction name Nothing
  , JSApp (JSFunction Nothing [name]
                      (JSBlock (concat $ mapMaybe (\decl -> fmap (map $ optimize opts) $ declToJs opts (ModuleName pname) decl env) (decls))))
          [JSAssignment (JSVar name)
                        (JSBinary Or (JSVar name) (JSObjectLiteral []))]
  ]

-- |
-- Generate code in the simplified Javascript intermediate representation for a declaration
--
declToJs :: Options -> ModuleName -> Declaration -> Environment -> Maybe [JS]
declToJs opts mp (ValueDeclaration ident _ _ val) e =
  Just $ JSVariableIntroduction (identToJs ident) (Just (valueToJs opts mp e val))
         : setProperty ident (var ident) mp
declToJs opts mp (BindingGroupDeclaration vals) e =
  Just $ concatMap (\(ident, val) ->
           JSVariableIntroduction (identToJs ident) (Just (valueToJs opts mp e val))
           : setProperty ident (var ident) mp
         ) vals
declToJs _ mp (DataDeclaration _ _ ctors) _ =
  Just $ flip concatMap ctors $ \(pn@(ProperName ctor), maybeTy) ->
    let
      ctorJs =
        case maybeTy of
          Nothing -> JSVariableIntroduction ctor (Just (JSObjectLiteral [ ("ctor", JSStringLiteral (show (Qualified (Just mp) pn))) ]))
          Just _ -> JSFunction (Just ctor) ["value"]
                      (JSBlock [JSReturn
                        (JSObjectLiteral [ ("ctor", JSStringLiteral (show (Qualified (Just mp) pn)))
                                         , ("value", JSVar "value") ])])
    in ctorJs : setProperty (Escaped ctor) (JSVar ctor) mp
declToJs opts mp (DataBindingGroupDeclaration ds) e =
  Just $ concat $ mapMaybe (flip (declToJs opts mp) e) ds
declToJs _ mp (ExternDeclaration importTy ident (Just js) _) _ =
  Just $ js : setProperty ident (var ident) mp
declToJs _ _ _ _ = Nothing

setProperty :: Ident -> JS -> ModuleName -> [JS]
setProperty ident@(Op op) val (ModuleName (ProperName moduleName)) =
  [ JSAssignment (accessor ident (JSVar moduleName)) val
  , JSAssignment (JSIndexer (JSStringLiteral op) (JSVar moduleName)) (accessor ident (JSVar moduleName)) ]
setProperty ident val (ModuleName (ProperName moduleName)) =
  [ JSAssignment (accessor ident (JSVar moduleName)) val ]

var :: Ident -> JS
var = JSVar . identToJs

accessor :: Ident -> JS -> JS
accessor (Ident name) | nameIsJsReserved name = JSIndexer (JSStringLiteral name)
accessor ident = JSAccessor (identToJs ident)

valueToJs :: Options -> ModuleName -> Environment -> Value -> JS
valueToJs _ _ _ (NumericLiteral n) = JSNumericLiteral n
valueToJs _ _ _ (StringLiteral s) = JSStringLiteral s
valueToJs _ _ _ (BooleanLiteral b) = JSBooleanLiteral b
valueToJs opts m e (ArrayLiteral xs) = JSArrayLiteral (map (valueToJs opts m e) xs)
valueToJs opts m e (ObjectLiteral ps) = JSObjectLiteral (map (second (valueToJs opts m e)) ps)
valueToJs opts m e (ObjectUpdate o ps) = JSApp (JSAccessor "extend" (JSVar "Object")) [ valueToJs opts m e o, JSObjectLiteral (map (second (valueToJs opts m e)) ps)]
valueToJs _ m e (Constructor (Qualified Nothing name)) =
  case M.lookup (m, name) (dataConstructors e) of
    Just (_, Alias aliasModule aliasIdent) -> qualifiedToJS id (Qualified (Just aliasModule) aliasIdent)
    _ -> JSVar . runProperName $ name
valueToJs _ _ _ (Constructor name) = qualifiedToJS (Ident . runProperName) name
valueToJs opts m e (Block sts) = JSApp (JSFunction Nothing [] (JSBlock (map (statementToJs opts m e) sts))) []
valueToJs opts m e (Case values binders) = bindersToJs opts m e binders (map (valueToJs opts m e) values)
valueToJs opts m e (IfThenElse cond th el) = JSConditional (valueToJs opts m e cond) (valueToJs opts m e th) (valueToJs opts m e el)
valueToJs opts m e (Accessor prop val) = JSAccessor prop (valueToJs opts m e val)
valueToJs opts m e (App val arg) = JSApp (valueToJs opts m e val) [valueToJs opts m e arg]
valueToJs opts m e (Abs arg val) = JSFunction Nothing [identToJs arg] (JSBlock [JSReturn (valueToJs opts m e val)])
valueToJs opts m e (TypedValue _ (Abs arg val) ty) | optionsPerformRuntimeTypeChecks opts = let arg' = identToJs arg in JSFunction Nothing [arg'] (JSBlock $ runtimeTypeChecks arg' ty ++ [JSReturn (valueToJs opts m e val)])
valueToJs _ m e (Var ident) = varToJs m e ident
valueToJs opts m e (TypedValue _ val _) = valueToJs opts m e val
valueToJs _ _ _ (TypeClassDictionary _ _) = error "Type class dictionary was not replaced"
valueToJs _ _ _ _ = error "Invalid argument to valueToJs"

runtimeTypeChecks :: String -> Type -> [JS]
runtimeTypeChecks arg ty =
  let
    argTy = getFunctionArgumentType ty
  in
    maybe [] (argumentCheck (JSVar arg)) argTy
  where
  getFunctionArgumentType :: Type -> Maybe Type
  getFunctionArgumentType (TypeApp (TypeApp t funArg) _) | t == tyFunction = Just funArg
  getFunctionArgumentType (ForAll _ ty' _) = getFunctionArgumentType ty'
  getFunctionArgumentType _ = Nothing
  argumentCheck :: JS -> Type -> [JS]
  argumentCheck val t | t == tyNumber = [typeCheck val "number"]
  argumentCheck val t | t == tyString = [typeCheck val "string"]
  argumentCheck val t | t == tyBoolean = [typeCheck val "boolean"]
  argumentCheck val (TypeApp t _) | t == tyArray = [arrayCheck val]
  argumentCheck val (Object row) =
    let
      (pairs, _) = rowToList row
    in
      typeCheck val "object" : concatMap (\(prop, ty') -> argumentCheck (JSAccessor prop val) ty') pairs
  argumentCheck val (TypeApp (TypeApp t _) _) | t == tyFunction = [typeCheck val "function"]
  argumentCheck val (ForAll _ ty' _) = argumentCheck val ty'
  argumentCheck _ _ = []
  typeCheck :: JS -> String -> JS
  typeCheck js ty' = JSIfElse (JSBinary NotEqualTo (JSTypeOf js) (JSStringLiteral ty')) (JSBlock [JSThrow (JSStringLiteral $ ty' ++ " expected")]) Nothing
  arrayCheck :: JS -> JS
  arrayCheck js = JSIfElse (JSUnary Not (JSApp (JSAccessor "isArray" (JSVar "Array")) [js])) (JSBlock [JSThrow (JSStringLiteral "Array expected")]) Nothing

varToJs :: ModuleName -> Environment -> Qualified Ident -> JS
varToJs m e qual@(Qualified _ ident) = go qual
  where
  go qual = case M.lookup (qualify m qual) (names e) of
    Just (_, ty) | isExtern ty -> var ident
    Just (_, Alias aliasModule aliasIdent) -> go (Qualified (Just aliasModule) aliasIdent)
    _ -> case qual of
           Qualified Nothing _ -> var ident
           _ -> qualifiedToJS id qual
  isExtern (Extern ForeignImport) = True
  isExtern (Alias m' ident') = case M.lookup (m', ident') (names e) of
    Just (_, ty') -> isExtern ty'
    Nothing -> error "Undefined alias in varToJs"
  isExtern _ = False

qualifiedToJS :: (a -> Ident) -> Qualified a -> JS
qualifiedToJS f (Qualified (Just (ModuleName (ProperName m))) a) = accessor (f a) (JSVar m)
qualifiedToJS f (Qualified Nothing a) = JSVar $ identToJs (f a)

bindersToJs :: Options -> ModuleName -> Environment -> [([Binder], Maybe Guard, Value)] -> [JS] -> JS
bindersToJs opts m e binders vals = runGen (map identToJs (unusedNames (binders, vals))) $ do
  valNames <- replicateM (length vals) fresh
  jss <- forM binders $ \(bs, grd, result) -> go valNames [JSReturn (valueToJs opts m e result)] bs grd
  return $ JSApp (JSFunction Nothing valNames (JSBlock (concat jss ++ [JSThrow (JSStringLiteral "Failed pattern match")])))
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
  return [JSIfElse (JSBinary EqualTo (JSVar varName) (JSStringLiteral str)) (JSBlock done) Nothing]
binderToJs _ _ varName done (NumberBinder num) =
  return [JSIfElse (JSBinary EqualTo (JSVar varName) (JSNumericLiteral num)) (JSBlock done) Nothing]
binderToJs _ _ varName done (BooleanBinder True) =
  return [JSIfElse (JSVar varName) (JSBlock done) Nothing]
binderToJs _ _ varName done (BooleanBinder False) =
  return [JSIfElse (JSUnary Not (JSVar varName)) (JSBlock done) Nothing]
binderToJs _ _ varName done (VarBinder ident) =
  return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : done)
binderToJs m e varName done (NullaryBinder ctor) =
  if isOnlyConstructor m e ctor
  then
    return done
  else
    return [JSIfElse (JSBinary EqualTo (JSAccessor "ctor" (JSVar varName)) (JSStringLiteral (show ((\(mp, nm) -> Qualified (Just mp) nm) $ canonicalizeDataConstructor m e ctor)))) (JSBlock done) Nothing]
binderToJs m e varName done (UnaryBinder ctor b) = do
  value <- fresh
  js <- binderToJs m e value done b
  let success = JSBlock (JSVariableIntroduction value (Just (JSAccessor "value" (JSVar varName))) : js)
  if isOnlyConstructor m e ctor
  then
    return [success]
  else
    return [JSIfElse (JSBinary EqualTo (JSAccessor "ctor" (JSVar varName)) (JSStringLiteral (show ((\(mp, nm) -> Qualified (Just mp) nm) $ canonicalizeDataConstructor m e ctor))))
                     success
                     Nothing]
binderToJs m e varName done (ObjectBinder bs) = go done bs
  where
  go :: [JS] -> [(String, Binder)] -> Gen [JS]
  go done' [] = return done'
  go done' ((prop, binder):bs') = do
    propVar <- fresh
    done'' <- go done' bs'
    js <- binderToJs m e propVar done'' binder
    return (JSVariableIntroduction propVar (Just (JSAccessor prop (JSVar varName))) : js)
binderToJs m e varName done (ArrayBinder bs) = do
  js <- go done 0 bs
  return [JSIfElse (JSBinary EqualTo (JSAccessor "length" (JSVar varName)) (JSNumericLiteral (Left (fromIntegral $ length bs)))) (JSBlock js) Nothing]
  where
  go :: [JS] -> Integer -> [Binder] -> Gen [JS]
  go done' _ [] = return done'
  go done' index (binder:bs') = do
    elVar <- fresh
    done'' <- go done' (index + 1) bs'
    js <- binderToJs m e elVar done'' binder
    return (JSVariableIntroduction elVar (Just (JSIndexer (JSNumericLiteral (Left index)) (JSVar varName))) : js)
binderToJs m e varName done (ConsBinder headBinder tailBinder) = do
  headVar <- fresh
  tailVar <- fresh
  js1 <- binderToJs m e headVar done headBinder
  js2 <- binderToJs m e tailVar js1 tailBinder
  return [JSIfElse (JSBinary GreaterThan (JSAccessor "length" (JSVar varName)) (JSNumericLiteral (Left 0))) (JSBlock
    ( JSVariableIntroduction headVar (Just (JSIndexer (JSNumericLiteral (Left 0)) (JSVar varName))) :
      JSVariableIntroduction tailVar (Just (JSApp (JSAccessor "slice" (JSVar varName)) [JSNumericLiteral (Left 1)])) :
      js2
    )) Nothing]
binderToJs m e varName done (NamedBinder ident binder) = do
  js <- binderToJs m e varName done binder
  return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : js)

isOnlyConstructor :: ModuleName -> Environment -> Qualified ProperName -> Bool
isOnlyConstructor m e ctor =
  let (ty, _) = fromMaybe (error "Data constructor not found") $ qualify m ctor `M.lookup` dataConstructors e
  in numConstructors ty == 1
  where
  numConstructors ty = length $ filter (\(ty1, _) -> ((==) `on` typeConstructor) ty ty1) $ M.elems $ dataConstructors e
  typeConstructor (TypeConstructor qual) = qualify m qual
  typeConstructor (ForAll _ ty _) = typeConstructor ty
  typeConstructor (TypeApp (TypeApp t _) ty) | t == tyFunction = typeConstructor ty
  typeConstructor (TypeApp ty _) = typeConstructor ty
  typeConstructor fn = error $ "Invalid arguments to typeConstructor: " ++ show fn

statementToJs :: Options -> ModuleName -> Environment -> Statement -> JS
statementToJs opts m e (VariableIntroduction ident value) = JSVariableIntroduction (identToJs ident) (Just (valueToJs opts m e value))
statementToJs opts m e (Assignment target value) = JSAssignment (JSVar (identToJs target)) (valueToJs opts m e value)
statementToJs opts m e (While cond sts) = JSWhile (valueToJs opts m e cond) (JSBlock (map (statementToJs opts m e) sts))
statementToJs opts m e (For ident start end sts) = JSFor (identToJs ident) (valueToJs opts m e start) (valueToJs opts m e end) (JSBlock (map (statementToJs opts m e) sts))
statementToJs opts m e (If ifst) = ifToJs ifst
  where
  ifToJs :: IfStatement -> JS
  ifToJs (IfStatement cond thens elses) = JSIfElse (valueToJs opts m e cond) (JSBlock (map (statementToJs opts m e) thens)) (fmap elseToJs elses)
  elseToJs :: ElseStatement -> JS
  elseToJs (Else sts) = JSBlock (map (statementToJs opts m e) sts)
  elseToJs (ElseIf elif) = ifToJs elif
statementToJs opts m e (Return value) = JSReturn (valueToJs opts m e value)
