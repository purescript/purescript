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
    moduleToJs,
    wrapExportsContainer,
    isIdent
) where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Function (on)

import Control.Arrow (second)
import Control.Monad (replicateM, forM)

import qualified Data.Map as M

import Language.PureScript.Names
import Language.PureScript.Scope
import Language.PureScript.Declarations
import Language.PureScript.CodeGen.Monad
import Language.PureScript.Options
import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.Types
import Language.PureScript.Optimizer
import Language.PureScript.CodeGen.Common
import Language.PureScript.Environment
import qualified Language.PureScript.Constants as C

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToJs :: Options -> Module -> Environment -> Maybe JS
moduleToJs opts (Module name decls (Just exps)) env =
  case jsDecls of
    [] -> Nothing
    _ -> Just $ JSAssignment (JSAccessor (moduleNameToJs name) (JSVar C._ps)) $
           JSApp (JSFunction Nothing [] (JSBlock $
            JSVariableIntroduction "module" (Just $ JSObjectLiteral []) :
            jsDecls ++
            jsExports ++
            [JSReturn $ JSVar "module"])) []
  where
  jsDecls = (concat $ mapMaybe (\decl -> fmap (map $ optimize opts) $ declToJs opts name decl env) decls)
  jsExports = concatMap exportToJs exps
moduleToJs _ _ _ = error "Exports should have been elaborated in name desugaring"

-- |
-- Generate code in the simplified Javascript intermediate representation for a declaration
--
declToJs :: Options -> ModuleName -> Declaration -> Environment -> Maybe [JS]
declToJs opts mp (ValueDeclaration ident _ _ _ val) e =
  Just [JSVariableIntroduction (identToJs ident) (Just (valueToJs opts mp e val))]
declToJs opts mp (BindingGroupDeclaration vals) e =
  Just $ flip concatMap vals $ \(ident, _, val) ->
    [JSVariableIntroduction (identToJs ident) (Just (valueToJs opts mp e val))]
declToJs _ mp (DataDeclaration _ _ ctors) _ =
  Just $ flip concatMap ctors $ \(pn@(ProperName ctor), tys) ->
    [JSVariableIntroduction ctor (Just (go pn 0 tys []))]
    where
    go :: ProperName -> Integer -> [Type] -> [JS] -> JS
    go pn _ [] values =
      JSObjectLiteral [ ("ctor", JSStringLiteral (show (Qualified (Just mp) pn))), ("values", JSArrayLiteral $ reverse values) ]
    go pn index (_ : tys') values =
      JSFunction Nothing ["value" ++ show index]
        (JSBlock [JSReturn (go pn (index + 1) tys' (JSVar ("value" ++ show index) : values))])
declToJs opts mp (DataBindingGroupDeclaration ds) e = Just $ concat $ mapMaybe (flip (declToJs opts mp) e) ds
declToJs _ _ (ExternDeclaration _ _ (Just js) _) _ = Just [js]
declToJs opts mp (PositionedDeclaration _ d) e = declToJs opts mp d e
declToJs _ _ _ _ = Nothing

-- |
-- Generate code in the simplified Javascript intermediate representation for an export from a
-- module.
--
exportToJs :: DeclarationRef -> [JS]
exportToJs (TypeRef _ (Just dctors)) = flip map dctors (export . Ident . runProperName)
exportToJs (ValueRef name) = [export name]
exportToJs (TypeInstanceRef name) = [export name]
exportToJs _ = []

-- |
-- Generate code in the simplified Javascript intermediate representation for assigning an exported
-- value to the current module object.
--
export :: Ident -> JS
export ident = JSAssignment (accessor ident (JSVar "module")) (var ident)

-- |
-- Generate code in the simplified Javascript intermediate representation for a variable based on a
-- PureScript identifier.
--
var :: Ident -> JS
var = JSVar . identToJs

-- |
-- Generate code in the simplified Javascript intermediate representation for an accessor based on
-- a PureScript identifier. If the name is not valid in Javascript (symbol based, reserved name) an
-- indexer is returned.
--
accessor :: Ident -> JS -> JS
accessor (Ident prop) = accessorString prop
accessor (Op op) = JSIndexer (JSStringLiteral op)

accessorString :: String -> JS -> JS
accessorString prop | isIdent prop = JSAccessor prop
                    | otherwise = JSIndexer (JSStringLiteral prop)

-- |
-- Generate code in the simplified Javascript intermediate representation for a value or expression.
--
valueToJs :: Options -> ModuleName -> Environment -> Value -> JS
valueToJs _ _ _ (NumericLiteral n) = JSNumericLiteral n
valueToJs _ _ _ (StringLiteral s) = JSStringLiteral s
valueToJs _ _ _ (BooleanLiteral b) = JSBooleanLiteral b
valueToJs opts m e (ArrayLiteral xs) = JSArrayLiteral (map (valueToJs opts m e) xs)
valueToJs opts m e (ObjectLiteral ps) = JSObjectLiteral (map (second (valueToJs opts m e)) ps)
valueToJs opts m e (ObjectUpdate o ps) = extendObj (valueToJs opts m e o) (map (second (valueToJs opts m e)) ps)
valueToJs _ m _ (Constructor name) = qualifiedToJS m (Ident . runProperName) name
valueToJs opts m e (Case values binders) = bindersToJs opts m e binders (map (valueToJs opts m e) values)
valueToJs opts m e (IfThenElse cond th el) = JSConditional (valueToJs opts m e cond) (valueToJs opts m e th) (valueToJs opts m e el)
valueToJs opts m e (Accessor prop val) = accessorString prop (valueToJs opts m e val)
valueToJs opts m e (App val arg) = JSApp (valueToJs opts m e val) [valueToJs opts m e arg]
valueToJs opts m e (Let ds val) = JSApp (JSFunction Nothing [] (JSBlock (concat (mapMaybe (flip (declToJs opts m) e) ds) ++ [JSReturn $ valueToJs opts m e val]))) []
valueToJs opts m e (Abs (Left arg) val) = JSFunction Nothing [identToJs arg] (JSBlock [JSReturn (valueToJs opts m (bindName m arg e) val)])
valueToJs opts m e (TypedValue _ (Abs (Left arg) val) ty) | optionsPerformRuntimeTypeChecks opts = let arg' = identToJs arg in JSFunction Nothing [arg'] (JSBlock $ runtimeTypeChecks arg' ty ++ [JSReturn (valueToJs opts m e val)])
valueToJs _ m _ (Var ident) = varToJs m ident
valueToJs opts m e (TypedValue _ val _) = valueToJs opts m e val
valueToJs opts m e (PositionedValue _ val) = valueToJs opts m e val
valueToJs _ _ _ (TypeClassDictionary _ _ _) = error "Type class dictionary was not replaced"
valueToJs _ _ _ _ = error "Invalid argument to valueToJs"

-- |
-- Shallow copy an object.
--
extendObj :: JS -> [(String, JS)] -> JS
extendObj obj sts = JSApp (JSFunction Nothing [] block) []
  where
  [newObj, key] = take 2 . map identToJs . unusedNames $ (obj, sts)
  jsKey = JSVar key
  jsNewObj = JSVar newObj
  block = JSBlock (objAssign:copy:extend ++ [JSReturn jsNewObj])
  objAssign = JSVariableIntroduction newObj (Just $ JSObjectLiteral [])
  copy = JSForIn key obj $ JSBlock [JSIfElse cond assign Nothing]
  cond = JSApp (JSAccessor "hasOwnProperty" obj) [jsKey]
  assign = JSBlock [JSAssignment (JSIndexer jsKey jsNewObj) (JSIndexer jsKey obj)]
  stToAssign (s, js) = JSAssignment (JSAccessor s jsNewObj) js
  extend = map stToAssign sts

-- |
-- Temporarily extends the environment with a single local variable name
--
bindName :: ModuleName -> Ident -> Environment -> Environment
bindName m ident = bindNames m [ident]

-- |
-- Temporarily extends the environment to include local variable names introduced by lambda
-- abstractions or case statements
--
bindNames :: ModuleName -> [Ident] -> Environment -> Environment
bindNames m idents env = env { names = M.fromList [ ((m, ident), (noType, LocalVariable)) | ident <- idents ] `M.union` names env }
  where
  noType = error "Temporary lambda variable type was read"


-- |
-- Generate code in the simplified Javascript intermediate representation for runtime type checks.
--
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
  argumentCheck val (TypeApp t row) | t == tyObject =
    let
      (pairs, _) = rowToList row
    in
      typeCheck val "object" : concatMap (\(prop, ty') -> argumentCheck (accessorString prop val) ty') pairs
  argumentCheck val (TypeApp (TypeApp t _) _) | t == tyFunction = [typeCheck val "function"]
  argumentCheck val (ForAll _ ty' _) = argumentCheck val ty'
  argumentCheck _ _ = []
  typeCheck :: JS -> String -> JS
  typeCheck js ty' = JSIfElse (JSBinary NotEqualTo (JSTypeOf js) (JSStringLiteral ty')) (JSBlock [JSThrow (JSStringLiteral $ ty' ++ " expected")]) Nothing
  arrayCheck :: JS -> JS
  arrayCheck js = JSIfElse (JSUnary Not (JSApp (JSAccessor "isArray" (JSVar "Array")) [js])) (JSBlock [JSThrow (JSStringLiteral "Array expected")]) Nothing

-- |
-- Generate code in the simplified Javascript intermediate representation for a reference to a
-- variable.
--
varToJs :: ModuleName -> Qualified Ident -> JS
varToJs _ (Qualified Nothing ident) = var ident
varToJs m qual = qualifiedToJS m id qual

-- |
-- Generate code in the simplified Javascript intermediate representation for a reference to a
-- variable that may have a qualified name.
--
qualifiedToJS :: ModuleName -> (a -> Ident) -> Qualified a -> JS
qualifiedToJS m f (Qualified (Just m') a) | m /= m' = accessor (f a) (JSAccessor (moduleNameToJs m') $ JSVar C._ps)
qualifiedToJS _ f (Qualified _ a) = JSVar $ identToJs (f a)

-- |
-- Generate code in the simplified Javascript intermediate representation for pattern match binders
-- and guards.
--
bindersToJs :: Options -> ModuleName -> Environment -> [CaseAlternative] -> [JS] -> JS
bindersToJs opts m e binders vals = runGen (map identToJs (unusedNames (binders, vals))) $ do
  valNames <- replicateM (length vals) fresh
  jss <- forM binders $ \(CaseAlternative bs grd result) -> go valNames [JSReturn (valueToJs opts m (bindNames m (binderNames bs) e) result)] bs grd
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

-- |
-- Generate code in the simplified Javascript intermediate representation for a pattern match
-- binder.
--
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
binderToJs m e varName done (ConstructorBinder ctor bs) = do
  js <- go 0 done bs
  if isOnlyConstructor e ctor
  then
    return js
  else
    return [JSIfElse (JSBinary EqualTo (JSAccessor "ctor" (JSVar varName)) (JSStringLiteral (show ctor)))
                     (JSBlock js)
                     Nothing]
  where
  go :: Integer -> [JS] -> [Binder] -> Gen [JS]
  go _ done' [] = return done'
  go index done' (binder:bs') = do
    argVar <- fresh
    done'' <- go (index + 1) done' bs'
    js <- binderToJs m e argVar done'' binder
    return (JSVariableIntroduction argVar (Just (JSIndexer (JSNumericLiteral (Left index)) (JSAccessor "values" (JSVar varName)))) : js)
binderToJs m e varName done (ObjectBinder bs) = go done bs
  where
  go :: [JS] -> [(String, Binder)] -> Gen [JS]
  go done' [] = return done'
  go done' ((prop, binder):bs') = do
    propVar <- fresh
    done'' <- go done' bs'
    js <- binderToJs m e propVar done'' binder
    return (JSVariableIntroduction propVar (Just (accessorString prop (JSVar varName))) : js)
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
binderToJs m e varName done (PositionedBinder _ binder) =
  binderToJs m e varName done binder

-- |
-- Checks whether a data constructor is the only constructor for that type, used to simplify the
-- check when generating code for binders.
--
isOnlyConstructor :: Environment -> Qualified ProperName -> Bool
isOnlyConstructor e ctor =
  let ty = fromMaybe (error "Data constructor not found") $ ctor `M.lookup` dataConstructors e
  in numConstructors (ctor, ty) == 1
  where
  numConstructors ty = length $ filter (((==) `on` typeConstructor) ty) $ M.toList $ dataConstructors e
  typeConstructor (Qualified (Just moduleName) _, (tyCtor, _)) = (moduleName, tyCtor)
  typeConstructor _ = error "Invalid argument to isOnlyConstructor"

wrapExportsContainer :: Options -> [JS] -> JS
wrapExportsContainer opts modules = JSApp (JSFunction Nothing [C._ps] $ JSBlock $ JSStringLiteral "use strict" : modules) [exportSelector]
  where
  exportSelector = JSConditional (JSBinary And (JSBinary NotEqualTo (JSTypeOf $ JSVar "module") (JSStringLiteral "undefined")) (JSAccessor "exports" (JSVar "module")))
                           (JSAccessor "exports" (JSVar "module"))
                           (JSConditional (JSBinary NotEqualTo (JSTypeOf $ JSVar "window") (JSStringLiteral "undefined"))
                             (JSAssignment (JSAccessor browserNamespace (JSVar "window")) (JSBinary Or (JSAccessor browserNamespace (JSVar "window")) (JSObjectLiteral [])))
                             (JSApp (JSFunction Nothing [] $ JSBlock [JSThrow $ JSStringLiteral "PureScript doesn't know how to export modules in the current environment"]) []))
  browserNamespace = optionsBrowserNamespace opts
