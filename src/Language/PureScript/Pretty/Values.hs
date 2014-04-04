-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Values
-- Copyright   :  Kinds.hs(c) Phil Freeman 2013
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

module Language.PureScript.Pretty.Values (
    prettyPrintValue,
    prettyPrintBinder
) where

import Data.Maybe (fromMaybe)
import Data.List (intercalate)

import Control.Arrow ((<+>), runKleisli)
import Control.PatternArrows
import Control.Monad.State
import Control.Applicative

import Language.PureScript.Declarations
import Language.PureScript.Pretty.Common
import Language.PureScript.Pretty.Types (prettyPrintType)

literals :: Pattern PrinterState Value String
literals = mkPattern' match
  where
  match :: Value -> StateT PrinterState Maybe String
  match (NumericLiteral n) = return $ either show show n
  match (StringLiteral s) = return $ show s
  match (BooleanLiteral True) = return "true"
  match (BooleanLiteral False) = return "false"
  match (ArrayLiteral xs) = fmap concat $ sequence
    [ return "[ "
    , withIndent $ prettyPrintMany prettyPrintValue' xs
    , return " ]"
    ]
  match (ObjectLiteral []) = return "{}"
  match (ObjectLiteral ps) = fmap concat $ sequence
    [ return "{\n"
    , withIndent $ prettyPrintMany prettyPrintObjectProperty ps
    , return "\n"
    , currentIndent
    , return "}"
    ]
  match (Constructor name) = return $ show name
  match (Case values binders) = fmap concat $ sequence
    [ return "case "
    , unwords <$> forM values prettyPrintValue'
    , return " of\n"
    , withIndent $ prettyPrintMany prettyPrintCaseAlternative binders
    , currentIndent
    ]
  match (Let ds val) = fmap concat $ sequence
    [ return "let\n"
    , withIndent $ prettyPrintMany prettyPrintDeclaration ds
    , return "\n"
    , currentIndent
    , return "in "
    , prettyPrintValue' val
    ]
  match (Var ident) = return $ show ident
  match (Do els) = fmap concat $ sequence
    [ return "do "
    , withIndent $ prettyPrintMany prettyPrintDoNotationElement els
    , currentIndent
    ]
  match (TypeClassDictionary _ _) = return "<<dict>>"
  match (TypedValue _ val _) = prettyPrintValue' val
  match (PositionedValue _ val) = prettyPrintValue' val
  match _ = mzero

prettyPrintDeclaration :: Declaration -> StateT PrinterState Maybe String
prettyPrintDeclaration (TypeDeclaration ident ty) = return $ show ident ++ " :: " ++ prettyPrintType ty
prettyPrintDeclaration (ValueDeclaration ident _ [] Nothing val) = fmap concat $ sequence
  [ return $ show ident ++ " = "
  , prettyPrintValue' val
  ]
prettyPrintDeclaration (PositionedDeclaration _ d) = prettyPrintDeclaration d
prettyPrintDeclaration _ = error "Invalid argument to prettyPrintDeclaration"

prettyPrintCaseAlternative :: CaseAlternative -> StateT PrinterState Maybe String
prettyPrintCaseAlternative (CaseAlternative binders grd val) =
  fmap concat $ sequence
    [ intercalate ", " <$> forM binders prettyPrintBinder'
    , maybe (return "") (fmap ("| " ++) . prettyPrintValue') grd
    , return " -> "
    , prettyPrintValue' val
    ]

prettyPrintDoNotationElement :: DoNotationElement -> StateT PrinterState Maybe String
prettyPrintDoNotationElement (DoNotationValue val) =
  prettyPrintValue' val
prettyPrintDoNotationElement (DoNotationBind binder val) =
  fmap concat $ sequence
    [ prettyPrintBinder' binder
    , return " <- "
    , prettyPrintValue' val
    ]
prettyPrintDoNotationElement (DoNotationLet ds) =
  fmap concat $ sequence
    [ return "let "
    , withIndent $ prettyPrintMany prettyPrintDeclaration ds
    ]
prettyPrintDoNotationElement (PositionedDoNotationElement _ el) = prettyPrintDoNotationElement el

ifThenElse :: Pattern PrinterState Value ((Value, Value), Value)
ifThenElse = mkPattern match
  where
  match (IfThenElse cond th el) = Just ((th, el), cond)
  match _ = Nothing

accessor :: Pattern PrinterState Value (String, Value)
accessor = mkPattern match
  where
  match (Accessor prop val) = Just (prop, val)
  match _ = Nothing

objectUpdate :: Pattern PrinterState Value ([String], Value)
objectUpdate = mkPattern match
  where
  match (ObjectUpdate o ps) = Just (flip map ps $ \(key, val) -> key ++ " = " ++ prettyPrintValue val, o)
  match _ = Nothing

app :: Pattern PrinterState Value (String, Value)
app = mkPattern match
  where
  match (App val arg) = Just (prettyPrintValue arg, val)
  match _ = Nothing

lam :: Pattern PrinterState Value (String, Value)
lam = mkPattern match
  where
  match (Abs (Left arg) val) = Just (show arg, val)
  match _ = Nothing

-- |
-- Generate a pretty-printed string representing an expression
--
prettyPrintValue :: Value -> String
prettyPrintValue = fromMaybe (error "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyPrintValue'

prettyPrintValue' :: Value -> StateT PrinterState Maybe String
prettyPrintValue' = runKleisli $ runPattern matchValue
  where
  matchValue :: Pattern PrinterState Value String
  matchValue = buildPrettyPrinter operators (literals <+> fmap parens matchValue)
  operators :: OperatorTable PrinterState Value String
  operators =
    OperatorTable [ [ Wrap accessor $ \prop val -> val ++ "." ++ prop ]
                  , [ Wrap objectUpdate $ \ps val -> val ++ "{ " ++ intercalate ", " ps ++ " }" ]
                  , [ Wrap app $ \arg val -> val ++ "(" ++ arg ++ ")" ]
                  , [ Split lam $ \arg val -> "\\" ++ arg ++ " -> " ++ prettyPrintValue val ]
                  , [ Wrap ifThenElse $ \(th, el) cond -> "if " ++ cond ++ " then " ++ prettyPrintValue th ++ " else " ++ prettyPrintValue el ]
                  ]

prettyPrintBinderAtom :: Pattern PrinterState Binder String
prettyPrintBinderAtom = mkPattern' match
  where
  match :: Binder -> StateT PrinterState Maybe String
  match NullBinder = return "_"
  match (StringBinder str) = return $ show str
  match (NumberBinder num) = return $ either show show num
  match (BooleanBinder True) = return "true"
  match (BooleanBinder False) = return "false"
  match (VarBinder ident) = return $ show ident
  match (ConstructorBinder ctor args) = fmap concat $ sequence
    [ return $ show ctor ++ " "
    , unwords <$> forM args match
    ]
  match (ObjectBinder bs) = fmap concat $ sequence
    [ return "{\n"
    , withIndent $ prettyPrintMany prettyPrintObjectPropertyBinder bs
    , currentIndent
    , return "}"
    ]
  match (ArrayBinder bs) = fmap concat $ sequence
    [ return "["
    , unwords <$> mapM prettyPrintBinder' bs
    , return "]"
    ]
  match (NamedBinder ident binder) = ((show ident ++ "@") ++) <$> prettyPrintBinder' binder
  match (PositionedBinder _ binder) = prettyPrintBinder' binder
  match _ = mzero

-- |
-- Generate a pretty-printed string representing a Binder
--
prettyPrintBinder :: Binder -> String
prettyPrintBinder = fromMaybe (error "Incomplete pattern") . flip evalStateT (PrinterState 0) . prettyPrintBinder'

prettyPrintBinder' :: Binder -> StateT PrinterState Maybe String
prettyPrintBinder' = runKleisli $ runPattern matchBinder
  where
  matchBinder :: Pattern PrinterState Binder String
  matchBinder = buildPrettyPrinter operators (prettyPrintBinderAtom <+> fmap parens matchBinder)
  operators :: OperatorTable PrinterState Binder String
  operators =
    OperatorTable [ [ AssocR matchConsBinder (\b1 b2 -> b1 ++ " : " ++ b2) ] ]

matchConsBinder :: Pattern PrinterState Binder (Binder, Binder)
matchConsBinder = mkPattern match'
  where
  match' (ConsBinder b1 b2) = Just (b1, b2)
  match' _ = Nothing

prettyPrintObjectPropertyBinder :: (String, Binder) -> StateT PrinterState Maybe String
prettyPrintObjectPropertyBinder (key, binder) = fmap concat $ sequence
    [ return $ key ++ ": "
    , prettyPrintBinder' binder
    ]

prettyPrintObjectProperty :: (String, Value) -> StateT PrinterState Maybe String
prettyPrintObjectProperty (key, value) = fmap concat $ sequence
    [ return $ key ++ ": "
    , prettyPrintValue' value
    ]
