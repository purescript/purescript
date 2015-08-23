-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Values
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Pretty printer for values
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Language.PureScript.Pretty.Values (
    prettyPrintValue,
    prettyPrintBinder,
    prettyPrintBinderAtom
) where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)

import Control.Arrow ((<+>), runKleisli, second)
import Control.PatternArrows
import Control.Monad.State
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Pretty.Common
import Language.PureScript.Pretty.Types (prettyPrintType, prettyPrintTypeAtom)

literals :: Pattern PrinterState Expr String
literals = mkPattern' match
  where
  match :: Expr -> StateT PrinterState Maybe String
  match (NumericLiteral n) = return $ either show show n
  match (StringLiteral s) = return $ show s
  match (CharLiteral c) = return $ show c
  match (BooleanLiteral True) = return "true"
  match (BooleanLiteral False) = return "false"
  match (ArrayLiteral xs) = return $ "[" ++ intercalate ", " (map prettyPrintValue xs) ++ "]"
  match (ObjectLiteral ps) = prettyPrintObject' $ second Just `map` ps
  match (ObjectConstructor ps) = prettyPrintObject' ps
  match (ObjectGetter prop) = return $ "(." ++ prop ++ ")"
  match (TypeClassDictionaryConstructorApp className ps) = concat <$> sequence
    [ return (show className ++ "(\n")
    , match ps
    , return ")"
    ]
  match (Constructor name) = return $ show name
  match (Case values binders) = concat <$> sequence
    [ return "case "
    , unwords <$> forM values prettyPrintValue'
    , return " of\n"
    , withIndent $ prettyPrintMany prettyPrintCaseAlternative binders
    , currentIndent
    ]
  match (Let ds val) = concat <$> sequence
    [ return "let\n"
    , withIndent $ prettyPrintMany prettyPrintDeclaration ds
    , return "\n"
    , currentIndent
    , return "in "
    , prettyPrintValue' val
    ]
  match (Var ident) = return $ show ident
  match (Do els) = concat <$> sequence
    [ return "do\n"
    , withIndent $ prettyPrintMany prettyPrintDoNotationElement els
    , currentIndent
    ]
  match (OperatorSection op (Right val)) = return $ "(" ++ prettyPrintValue op ++ " " ++ prettyPrintValue val ++ ")"
  match (OperatorSection op (Left val)) = return $ "(" ++ prettyPrintValue val ++ " " ++ prettyPrintValue op ++ ")"
  match (TypeClassDictionary (name, tys) _) = return $ "<<dict " ++ show name ++ " " ++ unwords (map prettyPrintTypeAtom tys) ++ ">>"
  match (SuperClassDictionary name _) = return $ "<<superclass dict " ++ show name ++ ">>"
  match (TypedValue _ val _) = prettyPrintValue' val
  match (PositionedValue _ _ val) = prettyPrintValue' val
  match _ = mzero

prettyPrintDeclaration :: Declaration -> StateT PrinterState Maybe String
prettyPrintDeclaration (TypeDeclaration ident ty) = return $ show ident ++ " :: " ++ prettyPrintType ty
prettyPrintDeclaration (ValueDeclaration ident _ [] (Right val)) = concat <$> sequence
  [ return $ show ident ++ " = "
  , prettyPrintValue' val
  ]
prettyPrintDeclaration (PositionedDeclaration _ _ d) = prettyPrintDeclaration d
prettyPrintDeclaration _ = error "Invalid argument to prettyPrintDeclaration"

prettyPrintCaseAlternative :: CaseAlternative -> StateT PrinterState Maybe String
prettyPrintCaseAlternative (CaseAlternative binders result) =
  concat <$> sequence
    [ return (unwords (map prettyPrintBinderAtom binders))
    , prettyPrintResult result
    ]
  where
  prettyPrintResult (Left gs) = concat <$> sequence
      [ return "\n"
      , withIndent $ prettyPrintMany prettyPrintGuardedValue gs
      ]
  prettyPrintResult (Right v) = (" -> " ++) <$> prettyPrintValue' v

  prettyPrintGuardedValue (grd, val) =
    concat <$> sequence
      [ return "| "
      , prettyPrintValue' grd
      , return " -> "
      , prettyPrintValue' val
      ]

prettyPrintDoNotationElement :: DoNotationElement -> StateT PrinterState Maybe String
prettyPrintDoNotationElement (DoNotationValue val) =
  prettyPrintValue' val
prettyPrintDoNotationElement (DoNotationBind binder val) =
  concat <$> sequence
    [ return (prettyPrintBinder binder)
    , return " <- "
    , prettyPrintValue' val
    ]
prettyPrintDoNotationElement (DoNotationLet ds) =
  concat <$> sequence
    [ return "let "
    , withIndent $ prettyPrintMany prettyPrintDeclaration ds
    ]
prettyPrintDoNotationElement (PositionedDoNotationElement _ _ el) = prettyPrintDoNotationElement el

prettyPrintObject' :: [(String, Maybe Expr)] -> StateT PrinterState Maybe String
prettyPrintObject' [] = return "{}"
prettyPrintObject' ps = return $ "{ " ++ intercalate ", " (map prettyPrintObjectProperty ps) ++ "}"
  where
  prettyPrintObjectProperty :: (String, Maybe Expr) -> String
  prettyPrintObjectProperty (key, value) = prettyPrintObjectKey key ++ ": " ++ maybe "_" prettyPrintValue value

ifThenElse :: Pattern PrinterState Expr ((Expr, Expr), Expr)
ifThenElse = mkPattern match
  where
  match (IfThenElse cond th el) = Just ((th, el), cond)
  match _ = Nothing

accessor :: Pattern PrinterState Expr (String, Expr)
accessor = mkPattern match
  where
  match (Accessor prop val) = Just (prop, val)
  match _ = Nothing

objectUpdate :: Pattern PrinterState Expr ([String], Expr)
objectUpdate = mkPattern match
  where
  match (ObjectUpdate o ps) = Just (flip map ps $ \(key, val) -> key ++ " = " ++ prettyPrintValue val, o)
  match (ObjectUpdater o ps) = Just (flip map ps $ \(key, val) -> key ++ " = " ++ maybe "_" prettyPrintValue val, fromMaybe (Var (Qualified Nothing $ Ident "_")) o)
  match _ = Nothing

app :: Pattern PrinterState Expr (String, Expr)
app = mkPattern match
  where
  match (App val arg) = Just (prettyPrintValue arg, val)
  match _ = Nothing

lam :: Pattern PrinterState Expr (String, Expr)
lam = mkPattern match
  where
  match (Abs (Left arg) val) = Just (show arg, val)
  match _ = Nothing

-- |
-- Generate a pretty-printed string representing an expression
--
prettyPrintValue :: Expr -> String
prettyPrintValue = fromMaybe (error "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyPrintValue'

prettyPrintValue' :: Expr -> StateT PrinterState Maybe String
prettyPrintValue' = runKleisli $ runPattern matchValue
  where
  matchValue :: Pattern PrinterState Expr String
  matchValue = buildPrettyPrinter operators (literals <+> fmap parens matchValue)
  operators :: OperatorTable PrinterState Expr String
  operators =
    OperatorTable [ [ Wrap accessor $ \prop val -> val ++ "." ++ prop ]
                  , [ Wrap objectUpdate $ \ps val -> val ++ "{ " ++ intercalate ", " ps ++ " }" ]
                  , [ Wrap app $ \arg val -> val ++ "(" ++ arg ++ ")" ]
                  , [ Split lam $ \arg val -> "\\" ++ arg ++ " -> " ++ prettyPrintValue val ]
                  , [ Wrap ifThenElse $ \(th, el) cond -> "if " ++ cond ++ " then " ++ prettyPrintValue th ++ " else " ++ prettyPrintValue el ]
                  ]

prettyPrintBinderAtom :: Binder -> String
prettyPrintBinderAtom NullBinder = "_"
prettyPrintBinderAtom (StringBinder str) = show str
prettyPrintBinderAtom (CharBinder c) = show c
prettyPrintBinderAtom (NumberBinder num) = either show show num
prettyPrintBinderAtom (BooleanBinder True) = "true"
prettyPrintBinderAtom (BooleanBinder False) = "false"
prettyPrintBinderAtom (VarBinder ident) = show ident
prettyPrintBinderAtom (ConstructorBinder ctor []) = show ctor
prettyPrintBinderAtom (ObjectBinder bs) = 
  "{ " 
  ++ intercalate ", " (map prettyPrintObjectPropertyBinder bs)
  ++ " }"
  where
  prettyPrintObjectPropertyBinder :: (String, Binder) -> String
  prettyPrintObjectPropertyBinder (key, binder) = prettyPrintObjectKey key ++ ": " ++ prettyPrintBinder binder
prettyPrintBinderAtom (ArrayBinder bs) =
  "[ " 
  ++ intercalate ", " (map prettyPrintBinder bs)
  ++ " ]"
prettyPrintBinderAtom (NamedBinder ident binder) = show ident ++ "@" ++ prettyPrintBinder binder
prettyPrintBinderAtom (PositionedBinder _ _ binder) = prettyPrintBinderAtom binder
prettyPrintBinderAtom b = parens (prettyPrintBinder b)

-- |
-- Generate a pretty-printed string representing a Binder
--
prettyPrintBinder :: Binder -> String
prettyPrintBinder (ConstructorBinder ctor []) = show ctor
prettyPrintBinder (ConstructorBinder ctor args) = show ctor ++ " " ++ unwords (map prettyPrintBinderAtom args)
prettyPrintBinder (PositionedBinder _ _ binder) = prettyPrintBinder binder
prettyPrintBinder b = prettyPrintBinderAtom b
