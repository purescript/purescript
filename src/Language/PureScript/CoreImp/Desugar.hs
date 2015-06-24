-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CoreFn.Desugar
-- Copyright   :  (c) 2013-14 Phil Freeman, (c) 2014 Gary Burgess, and other contributors
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>, Gary Burgess <gary.burgess@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- | The CoreFn -> CoreImp desugaring step
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PureScript.CoreImp.Desugar (moduleToCoreImp) where

import Control.Applicative
import Data.Traversable (traverse)
import Control.Monad (replicateM, forM, foldM)
import Control.Monad.Supply.Class

import Language.PureScript.Core
import Language.PureScript.CoreImp.AST
import Language.PureScript.CoreImp.Operators
import Language.PureScript.Names

import qualified Language.PureScript.Constants as C
import qualified Language.PureScript.CoreFn as CF

-- |
-- Desugars a module from CoreFn to CoreImp representation.
--
moduleToCoreImp :: forall m. (Applicative m, Monad m, MonadSupply m)
                => Module (CF.Bind Ann) -> m (Module (Decl Ann))
moduleToCoreImp (Module coms mn imps exps externs decls) =
  Module coms mn imps exps externs . concat <$> mapM bind decls

  where

  bind :: CF.Bind Ann -> m [Decl Ann]
  bind (CF.NonRec ident val) = fmap return (decl ident val)
  bind (CF.Rec vals) = uncurry decl `mapM` vals

  -- Desugar a declaration into a variable introduction or named function
  -- declaration.
  decl :: Ident -> CF.Expr Ann -> m (Decl Ann)
  decl ident d@(CF.Abs ann@(_, _, _, Just IsTypeClassConstructor) _ _) =
    let name = ProperName (runIdent ident)
    in return $ Constructor ann name ident (unapply [] d)
    where
    unapply :: [Ident] -> CF.Expr Ann -> [Ident]
    unapply args (CF.Abs _ arg body) = unapply (arg:args) body
    unapply args _ = reverse args
  decl ident (CF.Abs ann arg body) = Function ann ident [arg] <$> block body
  decl ident (CF.Constructor ann tyName args) =
    return $ Constructor ann tyName ident args
  decl ident val = VarDecl nullAnn ident <$> value val

  -- Desugar an CoreFn expression into a CoreImp expression or list of
  -- statements as necessary.
  expr :: CF.Expr Ann -> m (Either (Expr Ann) [Statement Ann])
  expr (CF.Literal ann lit) =
    Left . Literal ann <$> traverse value lit
  expr (CF.Accessor ann prop val) =
    Left . Accessor ann (str prop) <$> value val
  expr (CF.ObjectUpdate ann obj props) =
    Left <$> (ObjectUpdate ann <$> value obj <*> mapM (traverse value) props)
  expr (CF.Abs ann arg body) =
    Left . AnonFunction ann [arg] <$> block body
  expr (CF.App ann fn arg) =
    Left <$> (App ann <$> value fn <*> (return <$> value arg))
  expr (CF.Var ann ident) =
    return . Left $ Var ann ident
  expr (CF.Case _ val cs) =
    Right <$> cases val cs
  expr (CF.Let _ binds body) = do
    body' <- block body
    binds' <- concat <$> mapM bind binds
    return . Right $ Decl `map` binds' ++ body'
  expr (CF.Constructor{}) =
    error "Constructor in an unsupported position when desugaring CoreFn->CoreImp"

  -- Desugar a CoreFn expression into a block. Values are `return`ed.
  block :: CF.Expr Ann -> m [Statement Ann]
  block = fmap (either (return . Return nullAnn) id) . expr

  -- Desugar a CoreFn expression into an expression. Blocks are wrapped in an
  -- IIFE.
  value :: CF.Expr Ann -> m (Expr Ann)
  value = fmap (either id iife) . expr

  -- Creates an IIFE (immediately-invoked function expression) from a block so
  -- so `...stmnts...` becomes `(function () { ...stmnts... }())`.
  iife :: [Statement Ann] -> Expr Ann
  iife body = App nullAnn (AnonFunction nullAnn [] body) []

  cases :: [CF.Expr Ann] -> [CF.CaseAlternative Ann] -> m [Statement Ann]
  cases vals binders = do
    valNames <- replicateM (length vals) (Ident <$> freshName)
    assignments <- zipWith (VarDecl nullAnn) valNames <$> mapM value vals
    body <- forM binders $ \(CF.CaseAlternative bs result) -> do
      res <- guards result
      go valNames res bs
    return $ map Decl assignments ++ concat body ++ [Throw nullAnn "Pattern match failed"]
    where
      go :: [Ident] -> [Statement Ann] -> [CF.Binder Ann] -> m [Statement Ann]
      go _ done [] = return done
      go (v:vs) done (b:bs) = do
        done' <- go vs done bs
        binder v done' b
      go _ _ _ = error "Invalid arguments to bindersToJs"

      guards :: Either [(CF.Guard Ann, CF.Expr Ann)] (CF.Expr Ann) -> m [Statement Ann]
      guards (Left gs) = forM gs $ \(cond, val) ->
        IfElse nullAnn <$> value cond <*> block val <*> pure Nothing
      guards (Right v) = block v

  binder :: Ident -> [Statement Ann] -> CF.Binder Ann -> m [Statement Ann]
  binder _ done (CF.NullBinder _) = return done
  binder varName done (CF.LiteralBinder _ l) =
    literalBinder varName done l
  binder varName done (CF.VarBinder _ ident) =
    return $ Decl (VarDecl nullAnn ident (var varName)) : done
  binder varName done (CF.ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binder varName done b
  binder varName done (CF.ConstructorBinder (_, _, ty, Just (IsConstructor ctorType fields)) _ ctor bs) = do
    stmnts <- go (zip fields bs) done
    return $ case ctorType of
      ProductType -> stmnts
      SumType -> [IfElse nullAnn (IsTagOf (Nothing, [], ty, Nothing) ctor (var varName)) stmnts Nothing]
    where
    go :: [(Ident, CF.Binder Ann)] -> [Statement Ann] -> m [Statement Ann]
    go [] done' = return done'
    go ((field, b) : remain) done' = do
      argVar <- Ident <$> freshName
      done'' <- go remain done'
      stmnts <- binder argVar done'' b
      return $ Decl (VarDecl nullAnn argVar (Accessor nullAnn (str $ runIdent field) (var varName))) : stmnts
  binder varName done b@(CF.ConstructorBinder _ _ ctor _) | isCons ctor = do
    let (headBinders, tailBinder) = uncons [] b
        numberOfHeadBinders = fromIntegral $ length headBinders
    stmnts1 <- foldM (\done' (headBinder, index) -> do
      headVar <- Ident <$> freshName
      stmntss <- binder headVar done' headBinder
      return (Decl (VarDecl nullAnn headVar (Indexer nullAnn (int index) (var varName))) : stmntss)) done (zip headBinders [0..])
    tailVar <- Ident <$> freshName
    stmnts2 <- binder tailVar stmnts1 tailBinder
    return
      [
      IfElse nullAnn
        (BinaryOp nullAnn GreaterThanOrEqual (Accessor nullAnn (str "length") (var varName)) (int numberOfHeadBinders))
        (Decl (VarDecl nullAnn tailVar (App nullAnn (Accessor nullAnn (str "slice") (var varName)) [int numberOfHeadBinders])) : stmnts2)
        Nothing
      ]
    where
    uncons :: [CF.Binder Ann] -> CF.Binder Ann -> ([CF.Binder Ann], CF.Binder Ann)
    uncons acc (CF.ConstructorBinder _ _ ctor' [h, t]) | isCons ctor' = uncons (h : acc) t
    uncons acc tailBinder = (reverse acc, tailBinder)
  binder _ _ b@(CF.ConstructorBinder{}) =
    error $ "Invalid ConstructorBinder in binder: " ++ show b
  binder varName done (CF.NamedBinder _ ident b) = do
    stmnts <- binder varName done b
    return $ Decl (VarDecl nullAnn ident (var varName)) : stmnts

  literalBinder :: Ident -> [Statement Ann] -> Literal (CF.Binder Ann) -> m [Statement Ann]
  literalBinder varName done (NumericLiteral n) =
    return [IfElse nullAnn (BinaryOp nullAnn Equal (var varName) (Literal nullAnn $ NumericLiteral n)) done Nothing]
  literalBinder varName done (CharLiteral c) =
    return [IfElse nullAnn (BinaryOp nullAnn Equal (var varName) (str [c])) done Nothing]
  literalBinder varName done (StringLiteral s) =
    return [IfElse nullAnn (BinaryOp nullAnn Equal (var varName) (str s)) done Nothing]
  literalBinder varName done (BooleanLiteral True) =
    return [IfElse nullAnn (var varName) done Nothing]
  literalBinder varName done (BooleanLiteral False) =
    return [IfElse nullAnn (UnaryOp nullAnn Not (var varName)) done Nothing]
  literalBinder varName done (ObjectLiteral bs) = go done bs
    where
    go :: [Statement Ann] -> [(String, CF.Binder Ann)] -> m [Statement Ann]
    go done' [] = return done'
    go done' ((prop, b):bs') = do
      propVar <- Ident <$> freshName
      done'' <- go done' bs'
      stmnts <- binder propVar done'' b
      return $ Decl (VarDecl nullAnn propVar (Accessor nullAnn (str prop) (var varName))) : stmnts
  literalBinder varName done (ArrayLiteral bs) = do
    stmnts <- go done 0 bs
    return [IfElse nullAnn (BinaryOp nullAnn Equal (Accessor nullAnn (str "length") (var varName)) (int (fromIntegral $ length bs))) stmnts Nothing]
    where
    go :: [Statement Ann] -> Integer -> [CF.Binder Ann] -> m [Statement Ann]
    go done' _ [] = return done'
    go done' index (b:bs') = do
      elVar <- Ident <$> freshName
      done'' <- go done' (index + 1) bs'
      stmnts <- binder elVar done'' b
      return $ Decl (VarDecl nullAnn elVar (Indexer nullAnn (int index) (var varName))) : stmnts

var :: Ident -> Expr Ann
var = Var nullAnn . Qualified Nothing

str :: String -> Expr Ann
str = Literal nullAnn . StringLiteral

int :: Integer -> Expr Ann
int = Literal nullAnn . NumericLiteral . Left

isCons :: Qualified ProperName -> Bool
isCons (Qualified (Just mn) ctor) = mn == ModuleName [ProperName C.prim] && ctor == ProperName "Array"
isCons name = error $ "Unexpected argument in isCons: " ++ show name
