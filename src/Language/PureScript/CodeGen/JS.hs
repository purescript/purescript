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
    wrapExportsContainer
) where

import Data.Maybe (fromMaybe, mapMaybe)
import Data.Function (on)
import Data.Data (Data)
import Data.Generics (mkQ, everything)

import Control.Arrow (second)
import Control.Monad (replicateM, forM)

import qualified Data.Map as M

import Language.PureScript.TypeChecker (Environment(..), NameKind(..))
import Language.PureScript.Values
import Language.PureScript.Names
import Language.PureScript.Scope
import Language.PureScript.Declarations
import Language.PureScript.CodeGen.Monad
import Language.PureScript.Options
import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.Types
import Language.PureScript.CodeGen.Optimize
import Language.PureScript.CodeGen.Common

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToJs :: Options -> Module -> Environment -> Maybe JS
moduleToJs opts (Module name decls) env =
  case jsDecls of
    [] -> Nothing
    _ -> Just $ JSAssignment (JSAccessor (moduleNameToJs name) (JSVar "_ps")) $
           JSApp (JSFunction Nothing ["module"] (JSBlock $ jsDecls ++ [JSReturn $ JSVar "module"]))
                 [JSBinary Or (JSAccessor (moduleNameToJs name) (JSVar "_ps")) (JSObjectLiteral [])]
  where
  jsDecls = concat $ mapMaybe (\decl -> fmap (map $ optimize opts) $ declToJs opts name decl env) decls

-- |
-- Generate code in the simplified Javascript intermediate representation for a declaration
--
declToJs :: Options -> ModuleName -> Declaration -> Environment -> Maybe [JS]
declToJs opts mp (ValueDeclaration ident _ _ val) e =
  Just [ JSVariableIntroduction (identToJs ident) (Just (valueToJs opts mp e val))
         , setExportProperty ident (var ident) ]
declToJs opts mp (BindingGroupDeclaration vals) e =
  Just $ concatMap (\(ident, val) ->
           [ JSVariableIntroduction (identToJs ident) (Just (valueToJs opts mp e val))
           , setExportProperty ident (var ident) ]
         ) vals
declToJs _ mp (DataDeclaration _ _ ctors) _ =
  Just $ flip concatMap ctors $ \(pn@(ProperName ctor), tys) ->
    [ JSVariableIntroduction ctor (Just (go pn 0 tys []))
    , setExportProperty (Escaped ctor) (JSVar ctor) ]
    where
    go pn _ [] values =
      JSObjectLiteral [ ("ctor", JSStringLiteral (show (Qualified (Just mp) pn))), ("values", JSArrayLiteral $ reverse values) ]
    go pn index (_ : tys') values =
      JSFunction Nothing ["value" ++ show index]
        (JSBlock [JSReturn (go pn (index + 1) tys' (JSVar ("value" ++ show index) : values))])
declToJs opts mp (DataBindingGroupDeclaration ds) e =
  Just $ concat $ mapMaybe (flip (declToJs opts mp) e) ds
declToJs _ _ (ExternDeclaration _ ident (Just js) _) _ =
  Just [js, setExportProperty ident (var ident)]
declToJs _ _ _ _ = Nothing

-- |
-- Generate code in the simplified Javascript intermediate representation for exporting a
-- declaration from a module.
--
setExportProperty :: Ident -> JS -> JS
setExportProperty ident = JSAssignment (accessor ident (JSVar "module"))

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
accessor (Ident name) | nameIsJsReserved name = JSIndexer (JSStringLiteral name)
accessor (Op op) = JSIndexer (JSStringLiteral op)
accessor ident = JSAccessor (identToJs ident)

-- |
-- Generate code in the simplified Javascript intermediate representation for a value or expression.
--
valueToJs :: Options -> ModuleName -> Environment -> Value -> JS
valueToJs _ _ _ (NumericLiteral n) = JSNumericLiteral n
valueToJs _ _ _ (StringLiteral s) = JSStringLiteral s
valueToJs _ _ _ (BooleanLiteral b) = JSBooleanLiteral b
valueToJs opts m e (ArrayLiteral xs) = JSArrayLiteral (map (valueToJs opts m e) xs)
valueToJs opts m e (ObjectLiteral ps) = JSObjectLiteral (map (second (valueToJs opts m e)) ps)
valueToJs opts m e (ObjectUpdate o ps) = JSApp (JSAccessor "extend" (JSVar "Object")) [ valueToJs opts m e o, JSObjectLiteral (map (second (valueToJs opts m e)) ps)]
valueToJs _ m e (Constructor (Qualified Nothing name)) =
  case M.lookup (m, name) (dataConstructors e) of
    Just (_, Alias aliasModule aliasIdent) -> qualifiedToJS m id (Qualified (Just aliasModule) aliasIdent)
    _ -> JSVar . runProperName $ name
valueToJs _ m _ (Constructor name) = qualifiedToJS m (Ident . runProperName) name
valueToJs opts m e (Case values binders) = bindersToJs opts m e binders (map (valueToJs opts m e) values)
valueToJs opts m e (IfThenElse cond th el) = JSConditional (valueToJs opts m e cond) (valueToJs opts m e th) (valueToJs opts m e el)
valueToJs opts m e (Accessor prop val) = JSAccessor prop (valueToJs opts m e val)
valueToJs opts m e (App val arg) = JSApp (valueToJs opts m e val) [valueToJs opts m e arg]
valueToJs opts m e (Abs (Left arg) val) = JSFunction Nothing [identToJs arg] (JSBlock [JSReturn (valueToJs opts m (bindName m arg e) val)])
valueToJs opts m e (TypedValue _ (Abs (Left arg) val) ty) | optionsPerformRuntimeTypeChecks opts = let arg' = identToJs arg in JSFunction Nothing [arg'] (JSBlock $ runtimeTypeChecks arg' ty ++ [JSReturn (valueToJs opts m e val)])
valueToJs _ m e (Var ident) = varToJs m e ident
valueToJs opts m e (TypedValue _ val _) = valueToJs opts m e val
valueToJs _ _ _ (TypeClassDictionary _ _) = error "Type class dictionary was not replaced"
valueToJs _ _ _ _ = error "Invalid argument to valueToJs"

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

-- |
-- Generate code in the simplified Javascript intermediate representation for a reference to a
-- variable.
--
varToJs :: ModuleName -> Environment -> Qualified Ident -> JS
varToJs m e qual@(Qualified _ ident) = go qual
  where
  go qual' = case M.lookup (qualify m qual') (names e) of
    Just (_, ty) | isExtern ty -> var ident
    Just (_, Alias aliasModule aliasIdent) -> go (Qualified (Just aliasModule) aliasIdent)
    _ -> case qual' of
           Qualified Nothing _ -> var ident
           _ -> qualifiedToJS m id qual'
  isExtern (Extern ForeignImport) = True
  isExtern (Alias m' ident') = case M.lookup (m', ident') (names e) of
    Just (_, ty') -> isExtern ty'
    Nothing -> error "Undefined alias in varToJs"
  isExtern _ = False

-- |
-- Generate code in the simplified Javascript intermediate representation for a reference to a
-- variable that may have a qualified name.
--
qualifiedToJS :: ModuleName -> (a -> Ident) -> Qualified a -> JS
qualifiedToJS m f (Qualified (Just m') a) | m /= m' = accessor (f a) (JSAccessor (moduleNameToJs m') $ JSVar "_ps")
qualifiedToJS _ f (Qualified _ a) = JSVar $ identToJs (f a)

-- |
-- Generate code in the simplified Javascript intermediate representation for pattern match binders
-- and guards.
--
bindersToJs :: Options -> ModuleName -> Environment -> [([Binder], Maybe Guard, Value)] -> [JS] -> JS
bindersToJs opts m e binders vals = runGen (map identToJs (unusedNames (binders, vals))) $ do
  valNames <- replicateM (length vals) fresh
  jss <- forM binders $ \(bs, grd, result) -> go valNames [JSReturn (valueToJs opts m (bindNames m (binderNames bs) e) result)] bs grd
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
-- Collect all names introduced in binders in an expression
--
binderNames :: (Data d) => d -> [Ident]
binderNames = everything (++) (mkQ [] go)
  where
  go (VarBinder ident) = [ident]
  go _ = []

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
  if isOnlyConstructor m e ctor
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

-- |
-- Checks whether a data constructor is the only constructor for that type, used to simplify the
-- check when generating code for binders.
--
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

wrapExportsContainer :: Options -> [JS] -> JS
wrapExportsContainer opts modules = JSApp (JSFunction Nothing ["_ps"] $ JSBlock $ JSStringLiteral "use strict" : modules) [exportSelector]
  where
  exportSelector = JSConditional (JSBinary And (JSBinary NotEqualTo (JSTypeOf $ JSVar "module") (JSStringLiteral "undefined")) (JSAccessor "exports" (JSVar "module")))
                           (JSAccessor "exports" (JSVar "module"))
                           (JSConditional (JSBinary NotEqualTo (JSTypeOf $ JSVar "window") (JSStringLiteral "undefined"))
                             (JSAssignment (JSAccessor browserNamespace (JSVar "window")) (JSBinary Or (JSAccessor browserNamespace (JSVar "window")) (JSObjectLiteral [])))
                             (JSApp (JSFunction Nothing [] $ JSBlock [JSThrow $ JSStringLiteral "PureScript doesn't know how to export modules in the current environment"]) []))
  browserNamespace = optionsBrowserNamespace opts
