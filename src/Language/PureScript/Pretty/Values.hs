-- |
-- Pretty printer for values
--
module Language.PureScript.Pretty.Values
  ( prettyPrintValue
  , prettyPrintBinder
  , prettyPrintBinderAtom
  ) where

import Prelude.Compat

import Control.Arrow (second)

import Data.Text (Text)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Monoid as Monoid ((<>))
import qualified Data.Text as T

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Names
import Language.PureScript.Pretty.Common
import Language.PureScript.Pretty.Types (typeAsBox, typeAtomAsBox, prettyPrintObjectKey)
import Language.PureScript.Types (Constraint(..))
import Language.PureScript.PSString (PSString, prettyPrintString)

import Text.PrettyPrint.Boxes

-- TODO(Christoph): remove T.unpack s

textT :: Text -> Box
textT = text . T.unpack

-- | Render an aligned list of items separated with commas
list :: Char -> Char -> (a -> Box) -> [a] -> Box
list open close _ [] = text [open, close]
list open close f xs = vcat left (zipWith toLine [0 :: Int ..] xs ++ [ text [ close ] ])
  where
  toLine i a = text [ if i == 0 then open else ',', ' ' ] <> f a

ellipsis :: Box
ellipsis = text "..."

prettyPrintObject :: Int -> [(PSString, Maybe Expr)] -> Box
prettyPrintObject d = list '{' '}' prettyPrintObjectProperty
  where
  prettyPrintObjectProperty :: (PSString, Maybe Expr) -> Box
  prettyPrintObjectProperty (key, value) = textT (prettyPrintObjectKey key Monoid.<> ": ") <> maybe (text "_") (prettyPrintValue (d - 1)) value

prettyPrintUpdateEntry :: Int -> PSString -> Expr -> Box
prettyPrintUpdateEntry d key val = textT (prettyPrintObjectKey key) <> text " = " <> prettyPrintValue (d - 1) val

-- | Pretty-print an expression
prettyPrintValue :: Int -> Expr -> Box
prettyPrintValue d _ | d < 0 = text "..."
prettyPrintValue d (IfThenElse _ cond th el) =
  (text "if " <> prettyPrintValueAtom (d - 1) cond)
  // moveRight 2 (vcat left [ text "then " <> prettyPrintValueAtom (d - 1) th
                            , text "else " <> prettyPrintValueAtom (d - 1) el
                            ])
prettyPrintValue d (Accessor _ prop val) = prettyPrintValueAtom (d - 1) val `before` textT ("." Monoid.<> prettyPrintObjectKey prop)
prettyPrintValue d (ObjectUpdate _ o ps) = prettyPrintValueAtom (d - 1) o `beforeWithSpace` list '{' '}' (uncurry (prettyPrintUpdateEntry d)) ps
prettyPrintValue d (ObjectUpdateNested _ o ps) = prettyPrintValueAtom (d - 1) o `beforeWithSpace` prettyPrintUpdate ps
  where
    prettyPrintUpdate (PathTree tree) = list '{' '}' printNode (runAssocList tree)
    printNode (key, Leaf val) = prettyPrintUpdateEntry d key val
    printNode (key, Branch val) = textT (prettyPrintObjectKey key) `beforeWithSpace` prettyPrintUpdate val
prettyPrintValue d (App _ val arg) = prettyPrintValueAtom (d - 1) val `beforeWithSpace` prettyPrintValueAtom (d - 1) arg
prettyPrintValue d (Abs _ arg val) = text ('\\' : T.unpack (prettyPrintBinder arg) ++ " -> ") // moveRight 2 (prettyPrintValue (d - 1) val)
prettyPrintValue d (TypeClassDictionaryConstructorApp _ className ps) =
  text (T.unpack (runProperName (disqualify className)) ++ " ") <> prettyPrintValueAtom (d - 1) ps
prettyPrintValue d (Case _ values binders) =
  text "case " <> foldr (beforeWithSpace . prettyPrintValueAtom (d - 1)) (text "of") values //
    moveRight 2 (vcat left (map (prettyPrintCaseAlternative (d - 1)) binders))
prettyPrintValue d (Let _ ds val) =
  text "let" //
    moveRight 2 (vcat left (map (prettyPrintDeclaration (d - 1)) ds)) //
    (text "in " <> prettyPrintValue (d - 1) val)
prettyPrintValue d (Do _ els) =
  text "do " <> vcat left (map (prettyPrintDoNotationElement (d - 1)) els)
prettyPrintValue _ (TypeClassDictionary _ (Constraint name tys _) _ _) = foldl1 beforeWithSpace $ text ("#dict " ++ T.unpack (runProperName (disqualify name))) : map typeAtomAsBox tys
prettyPrintValue _ (DeferredDictionary _ name _) = text $ "#dict " ++ T.unpack (runProperName (disqualify name))
prettyPrintValue _ (TypeClassDictionaryAccessor _ className ident) =
    text "#dict-accessor " <> text (T.unpack (runProperName (disqualify className))) <> text "." <> text (T.unpack (showIdent ident)) <> text ">"
prettyPrintValue d (TypedValue _ _ val _) = prettyPrintValue d val
prettyPrintValue d (Literal _ l) = prettyPrintLiteralValue d l
prettyPrintValue _ (Hole _ name) = text "?" <> textT name
prettyPrintValue d expr@AnonymousArgument{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@Constructor{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@Var{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@Op{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@BinaryNoParens{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@Parens{} = prettyPrintValueAtom d expr
prettyPrintValue d expr@UnaryMinus{} = prettyPrintValueAtom d expr

-- | Pretty-print an atomic expression, adding parentheses if necessary.
prettyPrintValueAtom :: Int -> Expr -> Box
prettyPrintValueAtom d (Literal _ l) = prettyPrintLiteralValue d l
prettyPrintValueAtom _ AnonymousArgument{} = text "_"
prettyPrintValueAtom _ (Constructor _ name) = text . T.unpack $ runProperName (disqualify name)
prettyPrintValueAtom _ (Var _ ident) = text . T.unpack $ showIdent (disqualify ident)
prettyPrintValueAtom d (BinaryNoParens _ op lhs rhs) =
  prettyPrintValue (d - 1) lhs `beforeWithSpace` printOp op `beforeWithSpace` prettyPrintValue (d - 1) rhs
  where
  printOp (Op _ (Qualified _ name)) = text . T.unpack $ runOpName name
  printOp expr = text "`" <> prettyPrintValue (d - 1) expr `before` text "`"
prettyPrintValueAtom d (TypedValue _ _ val _) = prettyPrintValueAtom d val
prettyPrintValueAtom d (Parens _ expr) = (text "(" <> prettyPrintValue d expr) `before` text ")"
prettyPrintValueAtom d (UnaryMinus _ expr) = text "(-" <> prettyPrintValue d expr <> text ")"
prettyPrintValueAtom d expr = (text "(" <> prettyPrintValue d expr) `before` text ")"

prettyPrintLiteralValue :: Int -> Literal Expr -> Box
prettyPrintLiteralValue _ (NumericLiteral n) = text $ either show show n
prettyPrintLiteralValue _ (StringLiteral s) = text . T.unpack $ prettyPrintString s
prettyPrintLiteralValue _ (CharLiteral c) = text $ show c
prettyPrintLiteralValue _ (BooleanLiteral True) = text "true"
prettyPrintLiteralValue _ (BooleanLiteral False) = text "false"
prettyPrintLiteralValue d (ArrayLiteral xs) = list '[' ']' (prettyPrintValue (d - 1)) xs
prettyPrintLiteralValue d (ObjectLiteral ps) = prettyPrintObject (d - 1) $ second Just `map` ps

prettyPrintDeclaration :: Int -> Declaration -> Box
prettyPrintDeclaration d _ | d < 0 = ellipsis
prettyPrintDeclaration _ (TypeDeclaration td) =
  text (T.unpack (showIdent (tydeclIdent td)) ++ " :: ") <> typeAsBox (tydeclType td)
prettyPrintDeclaration d (ValueDecl _ ident _ [] [GuardedExpr _ [] val]) =
  text (T.unpack (showIdent ident) ++ " = ") <> prettyPrintValue (d - 1) val
prettyPrintDeclaration d (BindingGroupDeclaration ds) =
  vsep 1 left (NEL.toList (fmap (prettyPrintDeclaration (d - 1) . toDecl) ds))
  where
  toDecl ((sa@(ss, _), nm), t, e) = ValueDecl sa nm t [] [GuardedExpr ss [] e]
prettyPrintDeclaration _ _ = internalError "Invalid argument to prettyPrintDeclaration"

prettyPrintCaseAlternative :: Int -> CaseAlternative -> Box
prettyPrintCaseAlternative d _ | d < 0 = ellipsis
prettyPrintCaseAlternative d (CaseAlternative _ binders result) =
  text (T.unpack (T.unwords (map prettyPrintBinderAtom binders))) <> prettyPrintResult result
  where
  prettyPrintResult :: [GuardedExpr] -> Box
  prettyPrintResult [GuardedExpr _ [] v] = text " -> " <> prettyPrintValue (d - 1) v
  prettyPrintResult gs =
    vcat left (map (prettyPrintGuardedValueSep (text " | ")) gs)

  prettyPrintGuardedValueSep :: Box -> GuardedExpr -> Box
  prettyPrintGuardedValueSep _ (GuardedExpr _ [] val) =
    text " -> " <> prettyPrintValue (d - 1) val

  prettyPrintGuardedValueSep sep (GuardedExpr sa [guard] val) =
    foldl1 before [ sep
                  , prettyPrintGuard guard
                  , prettyPrintGuardedValueSep sep (GuardedExpr sa [] val)
                  ]

  prettyPrintGuardedValueSep sep (GuardedExpr sa (guard : guards) val) =
    vcat left [ foldl1 before
                [ sep
                , prettyPrintGuard guard
                ]
              , prettyPrintGuardedValueSep (text " , ") (GuardedExpr sa guards val)
              ]

  prettyPrintGuard (ConditionGuard _ cond) =
    prettyPrintValue (d - 1) cond
  prettyPrintGuard (PatternGuard _ binder val) =
    foldl1 before
    [ text (T.unpack (prettyPrintBinder binder))
    , text " <- "
    , prettyPrintValue (d - 1) val
    ]

prettyPrintDoNotationElement :: Int -> DoNotationElement -> Box
prettyPrintDoNotationElement d _ | d < 0 = ellipsis
prettyPrintDoNotationElement d (DoNotationValue _ val) =
  prettyPrintValue d val
prettyPrintDoNotationElement d (DoNotationBind _ binder val) =
  textT (prettyPrintBinder binder Monoid.<> " <- ") <> prettyPrintValue d val
prettyPrintDoNotationElement d (DoNotationLet _ ds) =
  text "let" //
    moveRight 2 (vcat left (map (prettyPrintDeclaration (d - 1)) ds))

prettyPrintBinderAtom :: Binder -> Text
prettyPrintBinderAtom NullBinder{} = "_"
prettyPrintBinderAtom (LiteralBinder _ l) = prettyPrintLiteralBinder l
prettyPrintBinderAtom (VarBinder _ ident) = showIdent ident
prettyPrintBinderAtom (ConstructorBinder _ ctor []) = runProperName (disqualify ctor)
prettyPrintBinderAtom b@ConstructorBinder{} = parensT (prettyPrintBinder b)
prettyPrintBinderAtom (NamedBinder _ ident binder) = showIdent ident Monoid.<> "@" Monoid.<> prettyPrintBinder binder
prettyPrintBinderAtom (TypedBinder _ _ binder) = prettyPrintBinderAtom binder
prettyPrintBinderAtom (OpBinder _ op) = runOpName (disqualify op)
prettyPrintBinderAtom (BinaryNoParensBinder _ op b1 b2) =
  prettyPrintBinderAtom b1 Monoid.<> " " Monoid.<> prettyPrintBinderAtom op Monoid.<> " " Monoid.<> prettyPrintBinderAtom b2
prettyPrintBinderAtom (ParensInBinder _ b) = parensT (prettyPrintBinder b)

prettyPrintLiteralBinder :: Literal Binder -> Text
prettyPrintLiteralBinder (StringLiteral str) = prettyPrintString str
prettyPrintLiteralBinder (CharLiteral c) = T.pack (show c)
prettyPrintLiteralBinder (NumericLiteral num) = either (T.pack . show) (T.pack . show) num
prettyPrintLiteralBinder (BooleanLiteral True) = "true"
prettyPrintLiteralBinder (BooleanLiteral False) = "false"
prettyPrintLiteralBinder (ObjectLiteral bs) =
  "{ "
  Monoid.<> T.intercalate ", " (map prettyPrintObjectPropertyBinder bs)
  Monoid.<> " }"
  where
  prettyPrintObjectPropertyBinder :: (PSString, Binder) -> Text
  prettyPrintObjectPropertyBinder (key, binder) = prettyPrintObjectKey key Monoid.<> ": " Monoid.<> prettyPrintBinder binder
prettyPrintLiteralBinder (ArrayLiteral bs) =
  "[ "
  Monoid.<> T.intercalate ", " (map prettyPrintBinder bs)
  Monoid.<> " ]"

-- |
-- Generate a pretty-printed string representing a Binder
--
prettyPrintBinder :: Binder -> Text
prettyPrintBinder (ConstructorBinder _ ctor []) = runProperName (disqualify ctor)
prettyPrintBinder (ConstructorBinder _ ctor args) = runProperName (disqualify ctor) Monoid.<> " " Monoid.<> T.unwords (map prettyPrintBinderAtom args)
prettyPrintBinder (TypedBinder _ _ binder) = prettyPrintBinder binder
prettyPrintBinder b = prettyPrintBinderAtom b
