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

import Data.List (intercalate)

import Control.Arrow (second)

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Names
import Language.PureScript.Pretty.Common
import Language.PureScript.Pretty.Types (typeAsBox, typeAtomAsBox)

import Text.PrettyPrint.Boxes

-- | Render an aligned list of items separated with commas
list :: Char -> Char -> (a -> Box) -> [a] -> Box
list open close _ [] = text [open, close]
list open close f xs = vcat left (zipWith toLine [0 :: Int ..] xs ++ [ text [ close ] ])
  where
  toLine i a = text [ if i == 0 then open else ',', ' ' ] <> f a

prettyPrintObject :: [(String, Maybe Expr)] -> Box
prettyPrintObject = list '{' '}' prettyPrintObjectProperty
  where
  prettyPrintObjectProperty :: (String, Maybe Expr) -> Box
  prettyPrintObjectProperty (key, value) = text (prettyPrintObjectKey key ++ ": ") <> maybe (text "_") prettyPrintValue value

-- | Pretty-print an expression
prettyPrintValue :: Expr -> Box
prettyPrintValue (IfThenElse cond th el) =
  (text "if " <> prettyPrintValueAtom cond)
  // moveRight 2 (vcat left [ text "then " <> prettyPrintValueAtom th
                            , text "else " <> prettyPrintValueAtom el
                            ])
prettyPrintValue (Accessor prop val) = prettyPrintValueAtom val <> text ("." ++ show prop)
prettyPrintValue (ObjectUpdate o ps) = prettyPrintValueAtom o <> text " " <> list '{' '}' (\(key, val) -> text (key ++ " = ") <> prettyPrintValue val) ps
prettyPrintValue (ObjectUpdater o ps) = maybe (text "_") prettyPrintValueAtom o <> text " " <> list '{' '}' (\(key, val) -> text (key ++ " = ") <> maybe (text "_") prettyPrintValue val) ps
prettyPrintValue (App val arg) = prettyPrintValueAtom val `beforeWithSpace` prettyPrintValueAtom arg
prettyPrintValue (Abs (Left arg) val) = text ('\\' : showIdent arg ++ " -> ") // moveRight 2 (prettyPrintValue val)
prettyPrintValue (TypeClassDictionaryConstructorApp className ps) =
  text (runProperName (disqualify className) ++ " ") <> prettyPrintValueAtom ps
prettyPrintValue (Case values binders) =
  (text "case " <> foldl1 beforeWithSpace (map prettyPrintValueAtom values) <> text " of") //
    moveRight 2 (vcat left (map prettyPrintCaseAlternative binders))
prettyPrintValue (Let ds val) =
  text "let" //
    moveRight 2 (vcat left (map prettyPrintDeclaration ds)) //
    (text "in " <> prettyPrintValue val)
prettyPrintValue (Do els) =
  text "do " <> vcat left (map prettyPrintDoNotationElement els)
prettyPrintValue (TypeClassDictionary (name, tys) _) = foldl1 beforeWithSpace $ text ("#dict " ++ runProperName (disqualify name)) : map typeAtomAsBox tys
prettyPrintValue (SuperClassDictionary name _) = text $ "#dict " ++ runProperName (disqualify name)
prettyPrintValue (TypedValue _ val _) = prettyPrintValue val
prettyPrintValue (PositionedValue _ _ val) = prettyPrintValue val
prettyPrintValue expr = prettyPrintValueAtom expr

-- | Pretty-print an atomic expression, adding parentheses if necessary.
prettyPrintValueAtom :: Expr -> Box
prettyPrintValueAtom (NumericLiteral n) = text $ either show show n
prettyPrintValueAtom (StringLiteral s) = text $ show s
prettyPrintValueAtom (CharLiteral c) = text $ show c
prettyPrintValueAtom (BooleanLiteral True) = text "true"
prettyPrintValueAtom (BooleanLiteral False) = text "false"
prettyPrintValueAtom (ArrayLiteral xs) = list '[' ']' prettyPrintValue xs
prettyPrintValueAtom (ObjectLiteral ps) = prettyPrintObject $ second Just `map` ps
prettyPrintValueAtom (ObjectConstructor ps) = prettyPrintObject ps
prettyPrintValueAtom (ObjectGetter prop) = text $ "_." ++ show prop
prettyPrintValueAtom (Constructor name) = text $ runProperName (disqualify name)
prettyPrintValueAtom (Var ident) = text $ showIdent (disqualify ident)
prettyPrintValueAtom (OperatorSection op (Right val)) = ((text "(" <> prettyPrintValue op) `beforeWithSpace` prettyPrintValue val) `before` text ")"
prettyPrintValueAtom (OperatorSection op (Left val)) = ((text "(" <> prettyPrintValue val) `beforeWithSpace` prettyPrintValue op) `before` text ")"
prettyPrintValueAtom (TypedValue _ val _) = prettyPrintValueAtom val
prettyPrintValueAtom (PositionedValue _ _ val) = prettyPrintValueAtom val
prettyPrintValueAtom expr = (text "(" <> prettyPrintValue expr) `before` text ")"

prettyPrintDeclaration :: Declaration -> Box
prettyPrintDeclaration (TypeDeclaration ident ty) =
  text (showIdent ident ++ " :: ") <> typeAsBox ty
prettyPrintDeclaration (ValueDeclaration ident _ [] (Right val)) =
  text (showIdent ident ++ " = ") <> prettyPrintValue val
prettyPrintDeclaration (BindingGroupDeclaration ds) =
  vsep 1 left (map (prettyPrintDeclaration . toDecl) ds)
  where
  toDecl (nm, t, e) = ValueDeclaration nm t [] (Right e)
prettyPrintDeclaration (PositionedDeclaration _ _ d) = prettyPrintDeclaration d
prettyPrintDeclaration _ = internalError "Invalid argument to prettyPrintDeclaration"

prettyPrintCaseAlternative :: CaseAlternative -> Box
prettyPrintCaseAlternative (CaseAlternative binders result) =
  text (unwords (map prettyPrintBinderAtom binders)) <> prettyPrintResult result
  where
  prettyPrintResult :: Either [(Guard, Expr)] Expr -> Box
  prettyPrintResult (Left gs) =
    vcat left (map prettyPrintGuardedValue gs)
  prettyPrintResult (Right v) = text " -> " <> prettyPrintValue v

  prettyPrintGuardedValue :: (Guard, Expr) -> Box
  prettyPrintGuardedValue (grd, val) = foldl1 before
    [ text " | "
    , prettyPrintValue grd
    , text " -> "
    , prettyPrintValue val
    ]

prettyPrintDoNotationElement :: DoNotationElement -> Box
prettyPrintDoNotationElement (DoNotationValue val) =
  prettyPrintValue val
prettyPrintDoNotationElement (DoNotationBind binder val) =
  text (prettyPrintBinder binder ++ " <- ") <> prettyPrintValue val
prettyPrintDoNotationElement (DoNotationLet ds) =
  text "let" //
    moveRight 2 (vcat left (map prettyPrintDeclaration ds))
prettyPrintDoNotationElement (PositionedDoNotationElement _ _ el) = prettyPrintDoNotationElement el

prettyPrintBinderAtom :: Binder -> String

prettyPrintBinderAtom NullBinder = "_"
prettyPrintBinderAtom (StringBinder str) = show str
prettyPrintBinderAtom (CharBinder c) = show c
prettyPrintBinderAtom (NumberBinder num) = either show show num
prettyPrintBinderAtom (BooleanBinder True) = "true"
prettyPrintBinderAtom (BooleanBinder False) = "false"
prettyPrintBinderAtom (VarBinder ident) = showIdent ident
prettyPrintBinderAtom (ConstructorBinder ctor []) = runProperName (disqualify ctor)
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
prettyPrintBinderAtom (NamedBinder ident binder) = showIdent ident ++ "@" ++ prettyPrintBinder binder
prettyPrintBinderAtom (PositionedBinder _ _ binder) = prettyPrintBinderAtom binder
prettyPrintBinderAtom b = parens (prettyPrintBinder b)

-- |
-- Generate a pretty-printed string representing a Binder
--
prettyPrintBinder :: Binder -> String
prettyPrintBinder (ConstructorBinder ctor []) = runProperName (disqualify ctor)
prettyPrintBinder (ConstructorBinder ctor args) = runProperName (disqualify ctor) ++ " " ++ unwords (map prettyPrintBinderAtom args)
prettyPrintBinder (PositionedBinder _ _ binder) = prettyPrintBinder binder
prettyPrintBinder (TypedBinder _ binder) = prettyPrintBinder binder
prettyPrintBinder b = prettyPrintBinderAtom b
